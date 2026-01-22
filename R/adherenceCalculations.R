#' Calculate adherence with sliding window approach (batched processing)
#'
#' Calculates adherence in batches for large datasets using a sliding window approach.
#'
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @inheritParams cmaDoc
#' @inheritParams medicationGroupDoc
#' @inheritParams batchSizeDoc
#' @inheritParams nameDoc
#' @param ... Additional arguments passed to AdhereR CMA_sliding_window functions.
#'
#' @return Database table reference containing adherence calculations with sliding windows.
#'
#' @export
calculateAdherenceSlidingWindowBatched <- function(drugExposure,
                                                   cdm,
                                                   name = "adherenceFromOMOP_results",
                                                   cma = c(
                                                     "CMA1",
                                                     "CMA2",
                                                     "CMA3",
                                                     "CMA4",
                                                     "CMA5",
                                                     "CMA6",
                                                     "CMA7",
                                                     "CMA8",
                                                     "CMA9"
                                                   ),
                                                   medicationGroup = NULL,
                                                   batchSize = 10000,
                                                   ...) {
  if (is.null(drugExposure)) {
    cli::cli_alert_warning("Provided drug exposure data is missing or faulty")
    return(NULL)
  }

  uniqueSubjectIds <- drugExposure %>%
    dplyr::distinct(person_id) %>%
    dplyr::pull(person_id)

  batches <- split(uniqueSubjectIds, ceiling(seq_along(uniqueSubjectIds) / batchSize))

  writeSchema <- CDMConnector::cdmWriteSchema(cdm)
  nameWithPrefix <- paste0(writeSchema[[2]], name)
  connection <- CDMConnector::cdmCon(cdm)

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))){
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return( NULL )
  }

  for (i in seq_along(batches)) {
    cli::cli_alert_info(paste("Working with batch", names(batches)[i], "out of", length(batches)))

    batch <- batches[[i]]
    batchData <- drugExposure %>%
      dplyr::filter(person_id %in% batch) %>%
      dplyr::collect()

    result <- calculateAdherenceSlidingWindow(
      drugExposure = batchData,
      cdm = NULL,
      cma = cma,
      medicationGroup = medicationGroup,
      ...
    )

    if (is.null(result)) {
      next
    }

    DBI::dbWriteTable(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix), value = result, append = TRUE)
  }

  if (is.null(result)) {
    return(NULL)
  }

  newTable <- dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = name, CDMConnector::dbms(connection)))

  return(newTable)
}

#' Calculate adherence using sliding window approach
#'
#' Calculates CMA (Continuous Medication Availability) metrics using a sliding window
#' approach to examine adherence over time within the observation period.
#'
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @inheritParams cmaDoc
#' @inheritParams medicationGroupDoc
#' @param delayObservationWindowStart (`logical(1)`) Should the observation window start
#'   be delayed until the first prescription? Default: `FALSE`.
#' @param ... Additional arguments passed to AdhereR CMA_sliding_window functions.
#'
#' @return Data frame with adherence calculations for each sliding window period.
#'   Includes window.id, window.start, window.end, and CMA values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")
#'
#' drugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   conceptSet = conceptSet,
#'   name = "drug_exposure"
#' )
#'
#' adherenceData <- drugExposure |>
#'   calculateAdherenceSlidingWindow(cdm, cma = c("CMA5", "CMA7"))
#' }
calculateAdherenceSlidingWindow <- function(drugExposure,
                                           cdm = NULL,
                                           cma = c(
                                             "CMA1",
                                             "CMA2",
                                             "CMA3",
                                             "CMA4",
                                             "CMA5",
                                             "CMA6",
                                             "CMA7",
                                             "CMA8",
                                             "CMA9"
                                           ),
                                           medicationGroup = NULL,
                                           delayObservationWindowStart = FALSE,
                                           ...) {
  if (!all(cma %in% c(
    "CMA1",
    "CMA2",
    "CMA3",
    "CMA4",
    "CMA5",
    "CMA6",
    "CMA7",
    "CMA8",
    "CMA9"
  ))) {
    cli::cli_alert_danger("Only CMAs 1-9 are expected.")
    return(NULL)
  }

  df <- loadData(cdm = cdm, name = NULL, data = drugExposure, delayObservationWindowStart = delayObservationWindowStart)

  if (is.null(df)) {
    return(NULL)
  }

  if (is.null(medicationGroup)) {
    medicationGroup <- list(unique(df$drug_concept_id))
    names(medicationGroup) <- c("all")
  }

  computedCMAs <- vector("list")

  for (i in cma) {
    cli::cli_alert_info("Calculating {i}.")
    cma_med <- vector()
    for (group in names(medicationGroup)) {
      currentGroup <- medicationGroup[[group]]
      data <- dplyr::filter(df, drug_concept_id %in% currentGroup)

      if (nrow(data) < 1) {
        next
      }
      tryCatch(
        {
          cmaSlidingWindowMedGroup <- AdhereR::CMA_sliding_window(
            CMA.to.apply = i,
            data = data,
            ID.colname = "person_id",
            event.date.colname = "drug_exposure_start_date",
            event.duration.colname = "days_supply",
            medication.class.colname = "drug_concept_id",
            followup.window.duration = "followup_period_duration",
            observation.window.duration = "observation_period_duration",
            observation.window.start = "observation_window_start",
            followup.window.start = "observation_period_start_date",
            ...
          )

          cmaSlidingWindowMedGroup <- dplyr::mutate(cmaSlidingWindowMedGroup$CMA, group = group)
        },
        error = function(e) {
          cli::cli_alert_danger("{i} failed: {e$message}")
          next
        }
      )
      cma_med <- rbind(cmaSlidingWindowMedGroup, cma_med)
    }
    computedCMAs[[i]] <- cma_med
  }

  cma_values <- dplyr::bind_rows(computedCMAs, .id = "name")

  cma_values <- cleanNARows(cma_values)

  result <- addPatientInformation(data = df, cma_values = cma_values)

  return(result)
}

#' Calculate adherence (batched processing)
#'
#' Calculates adherence in batches for large datasets.
#'
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @inheritParams cmaDoc
#' @inheritParams medicationGroupDoc
#' @inheritParams batchSizeDoc
#' @inheritParams nameDoc
#' @param ... Additional arguments passed to AdhereR CMA functions.
#'
#' @return Database table reference containing adherence calculations.
#'
#' @export
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")
#'
#' drugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   conceptSet = conceptSet,
#'   name = "drug_exposure"
#' )
#'
#' adherenceData <- drugExposure |>
#'   calculateAdherenceBatched(cdm, cma = "CMA5", name = "adherence_results")
#' }
#'
calculateAdherenceBatched <- function(drugExposure,
                                     cdm,
                                     name = "adherenceFromOMOP_results",
                                     cma = c(
                                       "CMA1",
                                       "CMA2",
                                       "CMA3",
                                       "CMA4",
                                       "CMA5",
                                       "CMA6",
                                       "CMA7",
                                       "CMA8",
                                       "CMA9"
                                     ),
                                     medicationGroup = NULL,
                                     batchSize = 1000,
                                     ...) {
  if (is.null(drugExposure)) {
    cli::cli_alert_warning("Provided drug exposure data is missing or faulty")
    return(NULL)
  }

  uniqueSubjectIds <- drugExposure %>%
    dplyr::distinct(person_id) %>%
    dplyr::pull(person_id)

  batches <- split(uniqueSubjectIds, ceiling(seq_along(uniqueSubjectIds) / batchSize))

  writeSchema <- CDMConnector::cdmWriteSchema(cdm)
  nameWithPrefix <- paste0(writeSchema[[2]], name)
  connection <- CDMConnector::cdmCon(cdm)

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))){
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return( NULL )
  }

  for (i in seq_along(batches)) {
    cli::cli_alert_info(paste("Working with batch", names(batches)[i], "out of", length(batches)))

    batch <- batches[[i]]
    batchData <- drugExposure %>%
      dplyr::filter(person_id %in% batch) %>%
      dplyr::collect()

    result <- calculateAdherence(
      drugExposure = batchData,
      cdm = NULL,
      cma = cma,
      medicationGroup = medicationGroup,
      ...
    )

    if (is.null(result)) {
      next
    }

    DBI::dbWriteTable(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix), value = result, append = TRUE)
  }

  if (is.null(result)) {
    return(NULL)
  }

  newTable <- dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = name, CDMConnector::dbms(connection)))

  return(newTable)
}

#' Calculate medication adherence
#'
#' Calculates CMA (Continuous Medication Availability) metrics for the duration
#' of the observation period.
#'
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @inheritParams cmaDoc
#' @inheritParams medicationGroupDoc
#' @param delayObservationWindowStart (`logical(1)`) Should the observation window start
#'   be delayed until the first prescription? Default: `FALSE`.
#' @param ... Additional arguments passed to AdhereR CMA functions.
#'
#' @returns Data frame with adherence calculations per person and medication group.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")
#'
#' drugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   conceptSet = conceptSet,
#'   name = "drug_exposure"
#' )
#'
#' adherenceData <- drugExposure |>
#'   calculateAdherence(cdm, cma = c("CMA5", "CMA7"))
#' }
calculateAdherence <- function(drugExposure,
                              cdm = NULL,
                              cma = c(
                                "CMA1",
                                "CMA2",
                                "CMA3",
                                "CMA4",
                                "CMA5",
                                "CMA6",
                                "CMA7",
                                "CMA8",
                                "CMA9"
                              ),
                              medicationGroup = NULL,
                              delayObservationWindowStart = FALSE,
                              ...) {
  if (!all(cma %in% c(
    "CMA1",
    "CMA2",
    "CMA3",
    "CMA4",
    "CMA5",
    "CMA6",
    "CMA7",
    "CMA8",
    "CMA9"
  ))) {
    cli::cli_alert_danger("Only CMAs 1-9 are expected.")
    return(NULL)
  }

  df <- loadData(cdm = cdm, name = NULL, data = drugExposure, delayObservationWindowStart = delayObservationWindowStart)
  if (is.null(df)) {
    return(NULL)
  }

  if (is.null(medicationGroup)) {
    medicationGroup <- list(unique(df$drug_concept_id))
    names(medicationGroup) <- c("all")
  }

  cma_med <- vector("list")
  computedCMAs <- vector("list")
  for (i in cma) {
    cli::cli_alert_info("Calculating { i }.")

    for (group in names(medicationGroup)) {
      # cli::cli_alert_info("Group {group}.")
      currentGroup <- medicationGroup[[group]]
      data <- dplyr::filter(df, drug_concept_id %in% currentGroup)

      if (nrow(data) < 1) {
        cli::cli_alert_info("Group {group} dataframe empty. Medication groups variable is a named list with drug concept ids and names")
        next
      }
      cmaAllMedGroup <- eval(rlang::parse_expr(
        paste0(
          "AdhereR::",
          i,
          '(
      data = data,
      ID.colname = "person_id",
      event.date.colname = "drug_exposure_start_date",
      event.duration.colname = "days_supply",
      medication.class.colname = "drug_concept_id",
      observation.window.start = "observation_window_start",
      observation.window.duration = "observation_period_duration",
      observation.window.duration.unit = "days",
      followup.window.duration = "followup_period_duration",
      followup.window.start = "observation_period_start_date",
      ...
    )'
        )
      ))

      cmaAllMedGroup <- dplyr::mutate(cmaAllMedGroup$CMA, group = group)

      cma_med <- rbind(cmaAllMedGroup, cma_med)
    }
    computedCMAs[[i]] <- cma_med
  }

  cma_values <- dplyr::bind_rows(computedCMAs, .id = "name")

  cma_values <- cleanNARows(cma_values)

  result <- addPatientInformation(data = df, cma_values = cma_values)

  return(result)
}

#' Adds patient information generated by generate chronic drug exposure function. User does not call this function directly.
#'
#' @param data input used for AdhereR functions, generated with generateChronicDrugExposure
#' @param cma_values output of AdhereR functions
#'
#' @returns cma values with patient information
#'
#' @examples
addPatientInformation <- function(data, cma_values) {
  # adhereR functions drop all patient profiles columns
  data <- data %>%
    dplyr::select(-days_supply, -drug_concept_id)

  if (all(c("window.start", "window.end") %in% colnames(cma_values))) {
    join_by <- dplyr::join_by(
      person_id,
      between(y$drug_exposure_start_date, x$window.start, x$window.end)
    )
  } else {
    join_by <- dplyr::join_by(person_id)
  }

  result <- cma_values %>%
    dplyr::left_join(data, join_by, relationship = "many-to-many") %>%
    dplyr::select(-drug_exposure_start_date) %>%
    dplyr::group_by(dplyr::across(c(-days_to_death))) %>%
    dplyr::filter(
      is.na(days_to_death) |
        days_to_death == max(days_to_death)
    ) %>%
    dplyr::distinct()

  return(result)
}

#' Loads data, performs basic checks and mutates columns. User does not call this function directly.
#'
#' @param cdm a cdm_reference object
#' @param name name of the table in database that contains output of generateChronicDrugExposure
#' @param data in-memory dataframe
#' @param delayObservationWindowStart boolean value. Delays the beginning of adhereR observation window. Useful when calculating adherence with sliding window approach.
#'
#' @returns verified dataframe in a suitable format
#'
#' @examples
loadData <- function(cdm, name, data, delayObservationWindowStart = FALSE) {
  if (!is.null(cdm)) {
    if (is.null(name)) {
      cli::cli_alert_danger("Table name must be provided.")
      return(NULL)
    }
    if (!name %in% CDMConnector::listTables(CDMConnector::cdmCon(cdm), schema = CDMConnector::cdmWriteSchema(cdm))) {
      cli::cli_alert_danger("Table { name } does not exist.")
      return(NULL)
    }
    df <- DBI::dbReadTable(CDMConnector::cdmCon(cdm), name = name)
  } else if (!is.null(data)) {
    df <- dplyr::collect(data)
    name <- "(provided data)"
  } else {
    cli::cli_alert_danger("No data provided.")
    return(NULL)
  }

  if (nrow(df) < 1 | is.null(df)) {
    cli::cli_alert_danger("Table { name } is empty.")

    return(NULL)
  }

  if (!all(
    c(
      "person_id",
      "drug_exposure_start_date",
      "days_supply",
      "drug_concept_id"
    ) %in% names(df)
  )) {
    cli::cli_alert_danger(
      "Mandatory column(s) missing. Columns person_id,
                          drug_exposure_start_date, days_supply, drug_concept_id must be present."
    )
    return(NULL)
  }

  if (all(c("cohort_start_date", "cohort_end_date") %in% colnames(df))) {
    # cohort period as followup period
    if (delayObservationWindowStart){
      df <- df %>%
        dplyr::group_by(person_id) %>%
        dplyr::mutate(
          observation_window_start = min(drug_exposure_start_date)
        ) %>%
        dplyr::mutate(
          observation_period_duration = as.integer(cohort_end_date - observation_window_start),
          followup_period_duration = as.numeric(
            observation_period_end_date - observation_period_start_date
          )
        )
      cli::cli_alert_info("Observation window start is delayed.")

    } else{
      # default
      df <- df %>%
        dplyr::mutate(
          observation_period_duration = as.integer(cohort_end_date - cohort_start_date),
          followup_period_duration = as.numeric(
            observation_period_end_date - observation_period_start_date
          )
        ) %>%
        dplyr::mutate(
          observation_window_start = cohort_start_date
        )
    }
  } else {
    if (delayObservationWindowStart){
      df <- df %>%
        dplyr::group_by(person_id) %>%
        dplyr::mutate(
          observation_window_start = min(drug_exposure_start_date)
        ) %>%
        dplyr::mutate(
          observation_period_duration = as.integer(
            observation_period_end_date - observation_window_start
          ),
          followup_period_duration = as.integer(
            observation_period_end_date - observation_period_start_date
          )
        )
      cli::cli_alert_info("Observation window start is delayed.")

    } else {
      df <- df %>%
        dplyr::mutate(
          observation_period_duration = as.integer(
            observation_period_end_date - observation_period_start_date
          ),
          followup_period_duration = as.integer(
            observation_period_end_date - observation_period_start_date
          )
        ) %>%
        dplyr::mutate(
          observation_window_start = observation_period_start_date
        )
    }
  }

  return(df)
}

#' cleanNARows
#'
#' User does not call this.
#' @param data
#'
#' @returns
#' \keyword{internal}
cleanNARows <- function(data){
  if ("window.ID" %in% colnames(data)){
  # rows with NA CMA values are dropped and window.ID reset after NA rows
  cleanedCMAData <- data %>%
    dplyr::group_by(name, person_id, group) %>%
    dplyr::mutate(
      nonNASeries = cumsum(is.na(CMA) | dplyr::lag(is.na(CMA), default = TRUE)),
      window.ID = ifelse(!is.na(CMA),window.ID,0)
    ) %>%
    dplyr::filter(!is.na(CMA)) %>%
    dplyr::group_by(nonNASeries) %>%
    dplyr::mutate(window.ID = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(!nonNASeries)
  } else {
    cleanedCMAData <- data %>%
      dplyr::filter(!is.na(CMA))
  }


  return(cleanedCMAData)
}
