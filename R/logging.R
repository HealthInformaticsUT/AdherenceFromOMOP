# Internal state of the optional run logger. Only active while a batched
# calculation with `logFile` is running; otherwise all log calls are no-ops.
.adherenceLogger <- new.env(parent = emptyenv())

#' Start the run logger
#'
#' @param logFile (`character(1)` or `NULL`) Path of the run log.
#' @param errorLogFile (`character(1)` or `NULL`) Path of the error log. If `NULL`,
#'   derived from `logFile` by adding `_errors` before the extension.
#'
#' @returns The previous logger state (a list), to be restored with [stopLogging()].
#'
#' @keywords internal
startLogging <- function(logFile = NULL, errorLogFile = NULL) {
  previous <- as.list(.adherenceLogger)

  if (is.null(logFile) && is.null(errorLogFile)) {
    return(previous)
  }
  if (!is.null(logFile) && is.null(errorLogFile)) {
    errorLogFile <- defaultErrorLogFile(logFile)
  }

  for (file in c(logFile, errorLogFile)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    # create the file up front so an empty error log means "no problems"
    if (!file.exists(file)) file.create(file)
  }

  .adherenceLogger$logFile <- logFile
  .adherenceLogger$errorLogFile <- errorLogFile
  .adherenceLogger$batch <- NULL
  .adherenceLogger$problemCount <- 0L

  if (!is.null(logFile)) cli::cli_alert_info("Writing run log to {.file {logFile}}.")
  if (!is.null(errorLogFile)) cli::cli_alert_info("Writing error log to {.file {errorLogFile}}.")

  previous
}

#' Restore the logger state saved by [startLogging()]
#'
#' @param previous List returned by [startLogging()].
#'
#' @keywords internal
stopLogging <- function(previous) {
  rm(list = ls(.adherenceLogger, all.names = TRUE), envir = .adherenceLogger)
  for (key in names(previous)) {
    assign(key, previous[[key]], envir = .adherenceLogger)
  }
  invisible(NULL)
}

#' @keywords internal
defaultErrorLogFile <- function(logFile) {
  if (grepl("\\.[[:alnum:]]+$", basename(logFile))) {
    sub("(\\.[[:alnum:]]+)$", "_errors\\1", logFile)
  } else {
    paste0(logFile, "_errors")
  }
}

#' @keywords internal
loggingActive <- function() {
  !is.null(.adherenceLogger$logFile) || !is.null(.adherenceLogger$errorLogFile)
}

#' @keywords internal
writeLogLines <- function(file, level, lines) {
  if (is.null(file)) {
    return(invisible(NULL))
  }
  batch <- .adherenceLogger$batch
  prefix <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", formatC(level, width = -7), "]",
    if (!is.null(batch)) paste0(" [batch ", batch, "]") else ""
  )
  cat(paste(prefix, lines), file = file, sep = "\n", append = TRUE)
  invisible(NULL)
}

#' Write a line to the run log
#'
#' @param ... Pieces pasted together into the message.
#'
#' @keywords internal
logInfo <- function(...) {
  writeLogLines(.adherenceLogger$logFile, "INFO", paste0(...))
}

#' Write a line to the error log
#'
#' @param level (`character(1)`) `"ERROR"` or `"WARNING"`.
#' @param message (`character`) Message text.
#'
#' @keywords internal
logProblem <- function(level, message) {
  if (!loggingActive()) {
    return(invisible(NULL))
  }
  message <- gsub("\n", " ", trimws(paste(message, collapse = " ")))
  .adherenceLogger$problemCount <- .adherenceLogger$problemCount + 1L
  writeLogLines(.adherenceLogger$errorLogFile, level, message)
}

#' Show a cli alert and record it in the error log
#'
#' @param text (`character(1)`) cli-formatted text, interpolated in `.envir`.
#' @param type (`character(1)`) `"danger"` or `"warning"`.
#' @param .envir Environment for interpolation.
#'
#' @keywords internal
alertProblem <- function(text, type = c("danger", "warning"), .envir = parent.frame()) {
  type <- match.arg(type)
  message <- cli::format_inline(text, .envir = .envir)
  if (type == "danger") {
    cli::cli_alert_danger("{message}")
    logProblem("ERROR", message)
  } else {
    cli::cli_alert_warning("{message}")
    logProblem("WARNING", message)
  }
  invisible(message)
}

#' @keywords internal
formatDuration <- function(seconds) {
  seconds <- round(as.numeric(seconds))
  sprintf("%02d:%02d:%02d", seconds %/% 3600, (seconds %% 3600) %/% 60, seconds %% 60)
}

#' Run the batch loop of the batched adherence functions
#'
#' Collects each batch, calculates adherence with `calculate` and appends the result
#' to the output table. When logging is active, writes batch information with
#' timestamps to the run log and warnings/errors to the error log.
#'
#' @param drugExposure Drug exposure table.
#' @param batches Named list of person_id vectors.
#' @param calculate Function taking the collected batch data and returning a data frame or NULL.
#' @param connection DBI connection.
#' @param tableId `DBI::Id` of the output table.
#'
#' @returns Number of rows written.
#'
#' @keywords internal
runBatches <- function(drugExposure, batches, calculate, connection, tableId) {
  runStart <- Sys.time()
  rowsWritten <- 0L

  for (i in seq_along(batches)) {
    cli::cli_alert_info(paste("Working with batch", names(batches)[i], "out of", length(batches)))
    .adherenceLogger$batch <- paste0(i, "/", length(batches))
    batchStart <- Sys.time()
    problemsBefore <- if (loggingActive()) .adherenceLogger$problemCount else 0L

    batch <- batches[[i]]
    batchData <- drugExposure %>%
      dplyr::filter(person_id %in% batch) %>%
      dplyr::collect()
    logInfo("Started: ", length(batch), " person(s), ", nrow(batchData), " drug exposure record(s).")

    result <- withCallingHandlers(
      calculate(batchData),
      warning = function(w) logProblem("WARNING", conditionMessage(w)),
      error = function(e) {
        logProblem("ERROR", conditionMessage(e))
        logInfo("Failed: ", conditionMessage(e))
      }
    )

    nRows <- if (is.null(result)) 0L else nrow(result)
    if (nRows > 0) {
      DBI::dbWriteTable(conn = connection, name = tableId, value = result, append = TRUE)
      rowsWritten <- rowsWritten + nRows
    }

    if (loggingActive()) {
      now <- Sys.time()
      elapsed <- difftime(now, runStart, units = "secs")
      remaining <- as.numeric(elapsed) / i * (length(batches) - i)
      logInfo(
        "Finished in ", formatDuration(difftime(now, batchStart, units = "secs")), ": ",
        if (nRows > 0) {
          paste0(dplyr::n_distinct(result$person_id), " person(s) with results, ", nRows, " row(s) written")
        } else {
          "no results"
        },
        ", ", .adherenceLogger$problemCount - problemsBefore, " problem(s) logged. ",
        "Elapsed ", formatDuration(elapsed), ", estimated remaining ", formatDuration(remaining), "."
      )
    }
  }
  .adherenceLogger$batch <- NULL

  rowsWritten
}

#' Log the start of a batched run
#'
#' @keywords internal
logRunStart <- function(functionName, tableName, nPersons, batches, batchSize, cma, medicationGroup, ...) {
  if (!loggingActive()) {
    return(invisible(NULL))
  }
  extra <- list(...)
  extraText <- if (length(extra) > 0) {
    paste0(names(extra), " = ", vapply(extra, function(x) paste(deparse(x), collapse = ""), character(1)), collapse = ", ")
  } else {
    "none"
  }
  logInfo("=== ", functionName, " started ===")
  logInfo("Output table: ", tableName)
  logInfo("CMA: ", paste(cma, collapse = ", "))
  logInfo(
    "Medication groups: ",
    if (is.null(medicationGroup)) "all (single group)" else paste(names(medicationGroup), collapse = ", ")
  )
  logInfo("Other arguments: ", extraText)
  logInfo(nPersons, " person(s) in ", length(batches), " batch(es) of up to ", batchSize, " person(s).")
}

#' Log the end of a batched run, including the result summary
#'
#' @keywords internal
logRunEnd <- function(functionName, runStart, rowsWritten, resultTable) {
  if (!loggingActive()) {
    return(invisible(NULL))
  }
  logInfo(
    "Run finished in ", formatDuration(difftime(Sys.time(), runStart, units = "secs")),
    ": ", rowsWritten, " row(s) written, ", .adherenceLogger$problemCount, " problem(s) in error log."
  )
  if (!is.null(resultTable) && rowsWritten > 0) {
    summary <- tryCatch(summariseAdherence(resultTable), error = function(e) {
      logProblem("ERROR", paste("Failed to summarise results:", conditionMessage(e)))
      NULL
    })
    if (!is.null(summary)) {
      logInfo("Result summary:")
      writeLogLines(.adherenceLogger$logFile, "INFO", formatAdherenceSummary(summary))
    }
  }
  logInfo("=== ", functionName, " finished ===")
}

#' Summarise an adherence results table
#'
#' Gives an overview of an adherence table produced by [calculateAdherence()],
#' [calculateAdherenceBatched()], [calculateAdherenceSlidingWindow()] or
#' [calculateAdherenceSlidingWindowBatched()]: the number of rows, distinct patients,
#' CMAs and medication groups, and the number of patients and rows per CMA and group.
#' Works on both in-memory data frames and database tables.
#'
#' @inheritParams adherenceDataDoc
#'
#' @returns A list with elements:
#'   * `overview`: one-row tibble with `rows`, `persons`, `cmas`, `groups`
#'     (and `windows_per_person_median` for sliding window results).
#'   * `byCma`: tibble with `persons` and `rows` per CMA (`name`).
#'   * `byGroup`: tibble with `persons`, `rows`, mean and median `CMA`
#'     per CMA (`name`) and `group`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' drugExposure <- generateChronicDrugExposure(cdm = cdm, name = "drug_exposure")
#' adherenceData <- calculateAdherence(drugExposure, cma = "CMA5")
#' summariseAdherence(adherenceData)
#' }
summariseAdherence <- function(adherenceData) {
  required <- c("name", "person_id", "group")
  missing <- setdiff(required, colnames(adherenceData))
  if (length(missing) > 0) {
    cli::cli_abort("Adherence data is missing column(s): {.field {missing}}.")
  }

  overview <- adherenceData %>%
    dplyr::summarise(
      rows = dplyr::n(),
      persons = dplyr::n_distinct(person_id),
      cmas = dplyr::n_distinct(name),
      groups = dplyr::n_distinct(group)
    ) %>%
    dplyr::collect()

  if ("window.ID" %in% colnames(adherenceData)) {
    windowsPerPerson <- adherenceData %>%
      dplyr::count(name, group, person_id) %>%
      dplyr::collect()
    overview$windows_per_person_median <- stats::median(windowsPerPerson$n)
  }

  byCma <- adherenceData %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(persons = dplyr::n_distinct(person_id), rows = dplyr::n(), .groups = "drop") %>%
    dplyr::collect() %>%
    dplyr::arrange(name)

  byGroup <- adherenceData %>%
    dplyr::group_by(name, group) %>%
    dplyr::summarise(
      persons = dplyr::n_distinct(person_id),
      rows = dplyr::n(),
      mean_cma = mean(CMA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect()
  # median is computed in R as not all databases support it in summarise
  medians <- adherenceData %>%
    dplyr::select(name, group, CMA) %>%
    dplyr::collect() %>%
    dplyr::group_by(name, group) %>%
    dplyr::summarise(median_cma = stats::median(CMA, na.rm = TRUE), .groups = "drop")
  byGroup <- byGroup %>%
    dplyr::left_join(medians, by = c("name", "group")) %>%
    dplyr::arrange(name, dplyr::desc(persons), group)

  list(
    overview = dplyr::mutate(overview, dplyr::across(dplyr::everything(), as.numeric)),
    byCma = byCma,
    byGroup = byGroup
  )
}

#' Format an adherence summary as text lines
#'
#' @param summary List returned by [summariseAdherence()].
#'
#' @returns Character vector of lines.
#'
#' @keywords internal
formatAdherenceSummary <- function(summary) {
  overview <- summary$overview
  lines <- c(
    paste0("  Rows: ", overview$rows),
    paste0("  Distinct persons: ", overview$persons),
    paste0("  CMAs: ", overview$cmas),
    paste0("  Groups: ", overview$groups)
  )
  if (!is.null(overview$windows_per_person_median)) {
    lines <- c(lines, paste0("  Median windows per person: ", overview$windows_per_person_median))
  }
  lines <- c(lines, "  Per CMA:")
  lines <- c(lines, sprintf("    %s: %s person(s), %s row(s)", summary$byCma$name, summary$byCma$persons, summary$byCma$rows))
  lines <- c(lines, "  Per CMA and group:")
  g <- summary$byGroup
  lines <- c(lines, sprintf(
    "    %s / %s: %s person(s), %s row(s), mean CMA %.3f, median CMA %.3f",
    g$name, g$group, g$persons, g$rows, g$mean_cma, g$median_cma
  ))
  lines
}
