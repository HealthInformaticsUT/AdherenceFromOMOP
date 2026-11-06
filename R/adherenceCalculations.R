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
#' @inheritParams loggingDoc
#' @param ... Additional arguments passed to [calculateAdherenceSlidingWindow()] (e.g. `delayObservationWindowStart`, `extendFollowupWindowStart`, `cleanRows`) and on to AdhereR CMA_sliding_window functions.
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
                                                   logFile = NULL,
                                                   errorLogFile = NULL,
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

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))) {
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return(NULL)
  }

  previousLogger <- startLogging(logFile, errorLogFile)
  on.exit(stopLogging(previousLogger), add = TRUE)
  runStart <- Sys.time()
  logRunStart(
    "calculateAdherenceSlidingWindowBatched", nameWithPrefix, length(uniqueSubjectIds),
    batches, batchSize, cma, medicationGroup, ...
  )

  rowsWritten <- runBatches(
    drugExposure = drugExposure,
    batches = batches,
    calculate = function(batchData) {
      calculateAdherenceSlidingWindow(
        drugExposure = batchData,
        cdm = NULL,
        cma = cma,
        medicationGroup = medicationGroup,
        ...
      )
    },
    connection = connection,
    tableId = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix)
  )

  newTable <- if (rowsWritten == 0) {
    NULL
  } else {
    tryCatch(
      {
        dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = name, CDMConnector::dbms(connection)))
      },
      error = function(e) {
        alertProblem("Failed to retrieve results table: {e$message}. Returning NULL.")
        NULL
      }
    )
  }

  logRunEnd("calculateAdherenceSlidingWindowBatched", runStart, rowsWritten, newTable)

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
#' @inheritParams extendFollowupWindowStartDoc
#' @param delayObservationWindowStart (`logical(1)`) Should the observation window start
#'   be delayed until the first prescription? Default: `FALSE`.
#' @param cleanRows (`logical(1)`) If TRUE, rows with NA CMA values are dropped and
#'   window.ID is reset to be sequential within each contiguous non-NA CMA interval.
#'   Default: `TRUE`.
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
                                            extendFollowupWindowStart = FALSE,
                                            cleanRows = TRUE,
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
    alertProblem("Only CMAs 1-9 are expected.")
    return(NULL)
  }

  df <- loadData(cdm = cdm, name = NULL, data = drugExposure, delayObservationWindowStart = delayObservationWindowStart, extendFollowupWindowStart = extendFollowupWindowStart)

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
    cma_med <- NULL
    for (group in names(medicationGroup)) {
      currentGroup <- medicationGroup[[group]]
      filteredData <- dplyr::filter(df, drug_concept_id %in% currentGroup)

      if (nrow(filteredData) < 1) {
        next
      }

      cmaSlidingWindowMedGroup <- tryCatch(
        {
          result <- AdhereR::CMA_sliding_window(
            CMA.to.apply = i,
            data = filteredData,
            ID.colname = "person_id",
            event.date.colname = "drug_exposure_start_date",
            event.duration.colname = "days_supply",
            medication.class.colname = "drug_concept_id",
            followup.window.duration = "followup_period_duration",
            observation.window.duration = "observation_period_duration",
            observation.window.start = "observation_window_start",
            followup.window.start = "followup_window_start",
            ...
          )
          if (is.null(result) || is.null(result$CMA)) {
            alertProblem("{i} for {group} returned no result from AdhereR (see AdhereR messages above).", type = "warning")
            NULL
          } else {
            dplyr::mutate(result$CMA, group = group)
          }
        },
        error = function(e) {
          alertProblem("{i} for {group} failed: {conditionMessage(e)}")
          NULL
        }
      )

      if (!is.null(cmaSlidingWindowMedGroup)) {
        cma_med <- rbind(cma_med, cmaSlidingWindowMedGroup)
      }
    }
    if (!is.null(cma_med) && nrow(cma_med) > 0) {
      computedCMAs[[i]] <- cma_med
    }
  }

  if (length(computedCMAs) > 0) {
    cma_values <- dplyr::bind_rows(computedCMAs, .id = "name")

    if (cleanRows) {
      cma_values <- cleanNARows(cma_values)
    }

    result <- addPatientInformation(data = df, cma_values = cma_values)
  } else {
    result <- NULL
  }

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
#' @inheritParams loggingDoc
#' @param ... Additional arguments passed to [calculateAdherence()] (e.g. `delayObservationWindowStart`, `extendFollowupWindowStart`, `cleanRows`) and on to AdhereR CMA functions.
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
                                      logFile = NULL,
                                      errorLogFile = NULL,
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

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))) {
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return(NULL)
  }

  previousLogger <- startLogging(logFile, errorLogFile)
  on.exit(stopLogging(previousLogger), add = TRUE)
  runStart <- Sys.time()
  logRunStart(
    "calculateAdherenceBatched", nameWithPrefix, length(uniqueSubjectIds),
    batches, batchSize, cma, medicationGroup, ...
  )

  rowsWritten <- runBatches(
    drugExposure = drugExposure,
    batches = batches,
    calculate = function(batchData) {
      calculateAdherence(
        drugExposure = batchData,
        cdm = NULL,
        cma = cma,
        medicationGroup = medicationGroup,
        ...
      )
    },
    connection = connection,
    tableId = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix)
  )

  newTable <- if (rowsWritten == 0) {
    NULL
  } else {
    dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = name, CDMConnector::dbms(connection)))
  }

  logRunEnd("calculateAdherenceBatched", runStart, rowsWritten, newTable)

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
#' @inheritParams extendFollowupWindowStartDoc
#' @param delayObservationWindowStart (`logical(1)`) Should the observation window start
#'   be delayed until the first prescription? Default: `FALSE`.
#' @param cleanRows (`logical(1)`) If TRUE, rows with NA CMA values are dropped.
#'   Default: `TRUE`.
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
                               extendFollowupWindowStart = FALSE,
                               cleanRows = TRUE,
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
    alertProblem("Only CMAs 1-9 are expected.")
    return(NULL)
  }

  df <- loadData(cdm = cdm, name = NULL, data = drugExposure, delayObservationWindowStart = delayObservationWindowStart, extendFollowupWindowStart = extendFollowupWindowStart)
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
    followup.window.start = "followup_window_start",
    ...
  )'
        )
      ))

      if (is.null(cmaAllMedGroup) || is.null(cmaAllMedGroup$CMA)) {
        alertProblem("{i} for {group} returned no result from AdhereR (see AdhereR messages above).", type = "warning")
        next
      }

      cmaAllMedGroup <- dplyr::mutate(cmaAllMedGroup$CMA, group = group)

      cma_med <- rbind(cmaAllMedGroup, cma_med)
    }
    computedCMAs[[i]] <- cma_med
  }

  cma_values <- dplyr::bind_rows(computedCMAs, .id = "name")

  if (cleanRows) {
    cma_values <- cleanNARows(cma_values)
  }
  result <- addPatientInformation(data = df, cma_values = cma_values)

  return(result)
}

#' Add patient information to CMA results
#'
#' Enriches CMA calculation results with patient demographic and observation period data.
#'
#' @param data Data frame containing drug exposure records with patient information,
#'   typically output from generateChronicDrugExposure.
#' @param cma_values Data frame containing CMA calculation results from AdhereR functions.
#'
#' @returns Data frame combining CMA results with patient information.
#'
#' @keywords internal
addPatientInformation <- function(data, cma_values) {
  data <- data %>%
    dplyr::select(-days_supply, -drug_concept_id, -drug_exposure_start_date, -dplyr::any_of("route_concept_id"))

  result <- cma_values %>%
    dplyr::left_join(data, dplyr::join_by(person_id)) %>%
    dplyr::distinct()

  return(result)
}

#' Load and prepare data for adherence calculations
#'
#' Loads drug exposure data from either a CDM database table or an in-memory data frame,
#' validates required columns, and computes observation window parameters needed by AdhereR.
#' Calculates followup_window_start, followup_period_duration, observation_window_start and
#' observation_period_duration based on cohort dates (if available) or observation period dates.
#' Without a cohort, the follow-up window is the observation period. If
#' extendFollowupWindowStart is TRUE, its start is moved back to cover drug exposures
#' dated before the observation period start.
#'
#' @param cdm A cdm_reference object created by CDMConnector. If provided, data is read
#'   from the database table specified by name. Either cdm+name or data must be provided.
#' @param name Character string specifying the table name in the CDM write schema containing
#'   drug exposure data generated by generateChronicDrugExposure. Required if cdm is provided.
#' @param data In-memory data frame or tibble with drug exposure records. Used when cdm is NULL.
#'   Must contain columns: person_id, drug_exposure_start_date, days_supply, drug_concept_id.
#' @param delayObservationWindowStart Logical. If TRUE, the observation window starts at the
#'   first drug exposure date for each patient instead of the observation/cohort start date.
#'   Useful for sliding window calculations to avoid empty initial windows. Default FALSE.
#' @param extendFollowupWindowStart Logical. If TRUE and no cohort is used, the follow-up
#'   window starts at the first drug exposure date when it precedes the observation period
#'   start. Default FALSE.
#'
#' @returns Data frame with original columns plus computed columns: followup_window_start,
#'   followup_period_duration, observation_window_start and observation_period_duration. Returns NULL if validation fails.
#'
#' @keywords internal
loadData <- function(cdm, name, data, delayObservationWindowStart = FALSE, extendFollowupWindowStart = FALSE) {
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
    alertProblem("Table { name } is empty.")

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
    # cohort period as observation window, observation period as follow-up window
    df <- df %>%
      dplyr::mutate(
        followup_window_start = observation_period_start_date,
        followup_period_duration = as.integer(
          observation_period_end_date - observation_period_start_date
        )
      )
    if (delayObservationWindowStart) {
      df <- df %>%
        dplyr::group_by(person_id) %>%
        dplyr::mutate(
          observation_window_start = min(drug_exposure_start_date)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          observation_period_duration = as.integer(cohort_end_date - observation_window_start)
        )
      cli::cli_alert_info("Observation window start is delayed.")
    } else {
      # default
      df <- df %>%
        dplyr::mutate(
          observation_period_duration = as.integer(cohort_end_date - cohort_start_date),
          observation_window_start = cohort_start_date
        )
    }
  } else {
    # Drug exposures may be dated before the recorded observation period start. AdhereR
    # rejects such persons when the observation window starts at the first exposure
    # (it is then not within the follow-up window), so optionally the follow-up window
    # start is moved back to the first exposure date.
    df <- df %>%
      dplyr::group_by(person_id) %>%
      dplyr::mutate(
        first_exposure_date = min(drug_exposure_start_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    earlyPersons <- df %>%
      dplyr::filter(first_exposure_date < observation_period_start_date) %>%
      dplyr::distinct(person_id) %>%
      nrow()

    if (extendFollowupWindowStart) {
      df <- df %>%
        dplyr::mutate(
          followup_window_start = pmin(observation_period_start_date, first_exposure_date)
        )
      if (earlyPersons > 0) {
        cli::cli_alert_info("{earlyPersons} person(s) have drug exposures before their observation period start; their follow-up window starts at the first drug exposure.")
      }
    } else {
      df <- df %>%
        dplyr::mutate(
          followup_window_start = observation_period_start_date
        )
      if (earlyPersons > 0 && delayObservationWindowStart) {
        alertProblem("{earlyPersons} person(s) have drug exposures before their observation period start and will fail in AdhereR. Set extendFollowupWindowStart = TRUE to start their follow-up window at the first drug exposure.", type = "warning")
      }
    }

    df <- df %>%
      dplyr::mutate(
        followup_period_duration = as.integer(observation_period_end_date - followup_window_start)
      )

    if (delayObservationWindowStart) {
      df <- df %>%
        dplyr::mutate(observation_window_start = first_exposure_date)
      cli::cli_alert_info("Observation window start is delayed.")
    } else {
      df <- df %>%
        dplyr::mutate(observation_window_start = followup_window_start)
    }

    df <- df %>%
      dplyr::mutate(
        observation_period_duration = as.integer(observation_period_end_date - observation_window_start)
      ) %>%
      dplyr::select(-first_exposure_date)
  }

  return(df)
}

#' Remove rows with NA CMA values and reset window IDs
#'
#' Filters out rows where CMA calculation returned NA and resets window.ID numbering
#' to be sequential within each contiguous block of non-NA results. This handles gaps
#' in sliding window calculations where some windows may not have sufficient data
#' for CMA computation.
#'
#' @param data Data frame containing CMA results with columns: name (CMA type),
#'   person_id, group, CMA, and optionally window.ID for sliding window results.
#'
#' @returns Data frame with NA CMA rows removed. For sliding window data, window.ID
#'   is reset to start from 1 within each contiguous series of non-NA values,
#'   grouped by name, person_id, and group.
#'
#' @keywords internal
cleanNARows <- function(data) {
  cmaValues <- data %>% dplyr::distinct(CMA) %>% dplyr::pull()


  if ("window.ID" %in% colnames(data) & !all(is.na(cmaValues))) {
    # rows with NA CMA values are dropped and window.ID reset after NA rows
    cleanedCMAData <- data %>%
      dplyr::group_by(name, person_id, group) %>%
      dplyr::mutate(
        nonNASeries = cumsum(is.na(CMA) | dplyr::lag(is.na(CMA), default = TRUE))
      ) %>%
      dplyr::filter(!is.na(CMA)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(name, person_id, group, nonNASeries) %>%
      dplyr::mutate(window.ID = 1:dplyr::n()) %>% #### TODO fails here
      dplyr::ungroup() %>%
      dplyr::select(!nonNASeries)
  } else {
    cleanedCMAData <- data %>%
      dplyr::filter(!is.na(CMA))
  }


  return(cleanedCMAData)
}

#' Function for grouping medications
#'
#' Uses CodelistGenerator to get drug ingredient codes and combines them into the
#' given ingredient groups. Optionally, only drugs with specific route categories are
#' included, and/or each group is further split by route category. Route categories
#' are derived by CodelistGenerator from the dose form of each drug concept, see
#' [CodelistGenerator::availableRouteCategories()] for the categories present in the
#' vocabulary.
#'
#' @inheritParams cdmDoc
#' @param ingredientGroups previously determined grouping, format as list(name = c(ingredient, ingredient)). Ingredient can be marked as concept ID or ingredient name.
#' @param groupByRoute (`logical(1)`) If `TRUE`, each ingredient group is split by
#'   route category, giving groups named `"{group}_{route_category}"` (e.g.
#'   `"statins_oral"`). Concepts without a known route end up in
#'   `"{group}_unclassified_route"`. Default: `FALSE`.
#' @param routeCategory (`character` or `NULL`) Route categories to include, e.g.
#'   `c("oral", "injectable")`. Drug concepts with other routes are left out. Passed
#'   to [CodelistGenerator::getDrugIngredientCodes()]. If `NULL`, all routes are
#'   included. Default: `NULL`.
#'
#' @returns codelist generator output with the structure of ingredientGroups
#'   (split by route if `groupByRoute = TRUE`). Groups without any concepts are dropped.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mock_cdm <- CodelistGenerator::mockVocabRef()
#' groups <- medicationGrouping(mock_cdm, ingredientGroups = list(group1 = c("Adalimumab")))
#'
#' # only injectable drugs, one group per route
#' groups <- medicationGrouping(
#'   mock_cdm,
#'   ingredientGroups = list(group1 = c("Adalimumab")),
#'   groupByRoute = TRUE,
#'   routeCategory = "injectable"
#' )
#' }
medicationGrouping <- function(cdm, ingredientGroups = NULL, groupByRoute = FALSE, routeCategory = NULL) {
  if (is.null(ingredientGroups)) {
    cli::cli_alert_info("Medication groups not provided")
    return(NULL)
  }

  if (!is.null(routeCategory)) {
    availableRoutes <- CodelistGenerator::availableRouteCategories(cdm)
    unknownRoutes <- setdiff(routeCategory, availableRoutes)
    if (length(unknownRoutes) > 0) {
      cli::cli_alert_warning("Route category not found in the vocabulary: {.val {unknownRoutes}}. Available: {.val {availableRoutes}}.")
    }
  }

  allIngredients <- unique(unlist(ingredientGroups, use.names = FALSE))
  ingredientsAreNumeric <- all(grepl("^\\d+$", as.character(allIngredients)))
  # concept ids are only matched by CodelistGenerator when given as numbers
  if (ingredientsAreNumeric) allIngredients <- as.numeric(allIngredients)
  ingredientCodelist <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm,
    name = allIngredients,
    nameStyle = if (ingredientsAreNumeric) "{concept_id}" else "{concept_name}",
    routeCategory = routeCategory
  )

  # CodelistGenerator names codelists in snake case ("Other ingredient" -> "other_ingredient")
  toCodelistName <- function(x) gsub("[^a-z0-9]+", "_", stringr::str_to_lower(trimws(x)))

  groupedCodelist <- list()
  for (groupName in names(ingredientGroups)) {
    ingredientsInGroup <- toCodelistName(ingredientGroups[[groupName]])

    # Find all codelist entries that match ingredients in this group
    matchingKeys <- names(ingredientCodelist)[toCodelistName(names(ingredientCodelist)) %in% ingredientsInGroup]

    # Combine and deduplicate concept IDs across matched ingredients
    conceptIds <- unique(unlist(ingredientCodelist[matchingKeys], use.names = FALSE))
    if (length(conceptIds) == 0) {
      cli::cli_alert_warning("No drug concepts found for group {.val {groupName}}{if (!is.null(routeCategory)) ' with the given route categories' else ''}. Group dropped.")
      next
    }
    groupedCodelist[[groupName]] <- conceptIds
  }

  groupedCodelist <- omopgenerics::newCodelist(groupedCodelist)

  if (groupByRoute && length(groupedCodelist) > 0) {
    groupedCodelist <- CodelistGenerator::stratifyByRouteCategory(groupedCodelist, cdm = cdm)
  }

  return(groupedCodelist)
}
