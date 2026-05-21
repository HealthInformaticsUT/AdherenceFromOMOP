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

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))) {
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return(NULL)
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

  newTable <- tryCatch(
    {
      dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = name, CDMConnector::dbms(connection)))
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to retrieve results table: {e$message}. Returning NULL.")
      NULL
    }
  )

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
            followup.window.start = "observation_period_start_date",
            ...
          )
          dplyr::mutate(result$CMA, group = group)
        },
        error = function(e) {
          cli::cli_alert_danger("{i} for {group} failed: {e$message}")
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

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))) {
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return(NULL)
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
    dplyr::select(-days_supply, -drug_concept_id, -drug_exposure_start_date)

  result <- cma_values %>%
    dplyr::left_join(data, dplyr::join_by(person_id)) %>%
    dplyr::distinct()

  return(result)
}

#' Load and prepare data for adherence calculations
#'
#' Loads drug exposure data from either a CDM database table or an in-memory data frame,
#' validates required columns, and computes observation window parameters needed by AdhereR.
#' Calculates observation_period_duration, followup_period_duration, and observation_window_start
#' based on cohort dates (if available) or observation period dates.
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
#'
#' @returns Data frame with original columns plus computed columns: observation_period_duration,
#'   followup_period_duration, and observation_window_start. Returns NULL if validation fails.
#'
#' @keywords internal
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
    if (delayObservationWindowStart) {
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
    } else {
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
    if (delayObservationWindowStart) {
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
  if ("window.ID" %in% colnames(data)) {
    # rows with NA CMA values are dropped and window.ID reset after NA rows
    cleanedCMAData <- data %>%
      dplyr::group_by(name, person_id, group) %>%
      dplyr::mutate(
        nonNASeries = cumsum(is.na(CMA) | dplyr::lag(is.na(CMA), default = TRUE))
      ) %>%
      dplyr::filter(!is.na(CMA)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(name, person_id, group, nonNASeries) %>%
      dplyr::mutate(window.ID = 1:dplyr::n()) %>%
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
#' Uses CodelistGenerator to get drug ingredient codes
#'
#' @inheritParams cdmDoc
#' @param ingredientGroups previously determined grouping, format as list(name = c(ingredient, ingredient)). Ingredient can be marked as concept ID or ingredient name.
#'
#' @returns codelist generator output with the structure of ingredientGroups
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mock_cdm <- mockDrugExposure()
#' groups <- medicationGrouping(mock_cdm, ingredientGroups=list(group1 = c(21216049, 35741956), group2 = c(37498042, 42899580)))
#'
#' }
medicationGrouping <- function(cdm, ingredientGroups = NULL){

  if (is.null(ingredientGroups)){
    cli::cli_alert_info("Medication groups not provided")
    return(NULL)
  }

  allIngredients <- unlist(ingredientGroups, use.names=FALSE)
  ingredientsAreNumeric <- all(grepl("^\\d+$", as.character(allIngredients)))
  ingredientCodelist  <- CodelistGenerator::getDrugIngredientCodes(
    cdm = cdm,
    name = allIngredients,
    nameStyle = if (ingredientsAreNumeric) "{concept_id}" else "{concept_name}"
  )
  groupedCodelist <- list()
  for (groupName in names(ingredientGroups)) {
    ingredientsInGroup <- stringr::str_to_lower(ingredientGroups[[groupName]])

    # Find all codelist entries that match ingredients in this group
    matchingKeys <- names(ingredientCodelist)[names(ingredientCodelist) %in% ingredientsInGroup]

    # Combine and deduplicate concept IDs across matched ingredients
    groupedCodelist[[groupName]] <- unique(unlist(ingredientCodelist[matchingKeys], use.names = FALSE))
  }

  groupedCodelist <- omopgenerics::newCodelist(groupedCodelist)

  return( groupedCodelist )
}

