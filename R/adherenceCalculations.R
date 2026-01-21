#' For using adherence sliding window function with large datasets
#'
#' Calculates adherence in batches
#'
#' @param reference reference to table
#' @param cdm a cdm_reference object
#' @param ... input values for CMA_sliding_window
#' @param batchSize default 1000.
#' @param medicationGroup CMA is calculated for each defined group. Medication group is a named list with a name and drug_concept_id values. For creating this list it is highly recommended to use CodelistGenerator \link[CodelistGenerator]{"https://darwin-eu.github.io/CodelistGenerator/articles/a04_GenerateVocabularyBasedCodelist.html"}
#' @param cma By default the function calculates adherence with all different CMA algorithms (CMA1-CMA9).
#' @param resultsTableName name of the table where results are stores
#'
#' @export
adherenceSlidingWindowLoader <- function(reference,
                                         cdm,
                                         resultsTableName = "adherenceFromOMOP_results",
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
  if (is.null(reference)) {
    cli::cli_alert_warning("Provided reference is faulty")
    return(NULL)
  }

  uniqueSubjectIds <- reference %>%
    dplyr::distinct(person_id) %>%
    dplyr::pull(person_id)

  batches <- split(uniqueSubjectIds, ceiling(seq_along(uniqueSubjectIds) / batchSize))

  writeSchema <- CDMConnector::cdmWriteSchema(cdm)
  nameWithPrefix <- paste0(writeSchema[[2]], resultsTableName)
  connection <- CDMConnector::cdmCon(cdm)

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))){
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return( NULL )
  }

  for (i in seq_along(batches)) {
    cli::cli_alert_info(paste("Working with batch", names(batches)[i], "out of", length(batches)))

    batch <- batches[[i]]
    data <- reference %>%
      dplyr::filter(person_id %in% batch) %>%
      dplyr::collect()

    result <- adherenceSlidingWindow(
      cdm = NULL,
      data = data,
      cma = cma,
      name = NULL,
      medicationGroup = medicationGroup,
      ...
    )

    if (is.null(result)) {
      next
    }

    DBI::dbWriteTable(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix), value = result, append = TRUE)
  }

  newTable <- tryCatch({
     dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = resultsTableName, CDMConnector::dbms(connection)))
  }, error = function(e) {
    newTable <- NULL
    cli::cli_alert_danger("{i} failed: {e$message}. Returning null.")
    NULL
  })

  return(newTable)
}

#' AdhereR sliding window function wrapper for OMOP CDM
#'
#' @param cdm a cdm_reference object with 'name' table
#' @param name name of the table used for adherence calculations, generated with generateChronicDrugExposure()
#' @param ... input values for CMA_sliding_window
#' @param cma By default the function calculates adherence with all different CMA algorithms (CMA1-CMA9).
#' @param data Can be used with in-memory data.
#' @param medicationGroup CMA is calculated for each defined group. Medication group is a named list with a name and drug_concept_id values. For creating this list it is highly recommended to use CodelistGenerator \link[CodelistGenerator]{"https://darwin-eu.github.io/CodelistGenerator/articles/a04_GenerateVocabularyBasedCodelist.html"}
#' @param delayObservationWindowStart delays adherence calculation window until first medication purchase
#' @param cleanRows If TRUE, rows with NA CMA values are dropped and window.ID is reset to be sequential within each contiguous non-NA CMA interval.
#'
#' @return vector with CMA_sliding_windows results
#' @export
#'
#' @examples
#' \dontrun{
#' connection <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomiaDir()))
#'
#' cdm <- CDMConnector::cdmFromCon(
#'   con = connection,
#'   cdmSchema = "main",
#'   writeSchema = "main",
#'   cdmName = "my_duckdb_database"
#' )
#'
#' cdm <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   ingredients = c("acetaminophen", "metformin", "simvastatin"),
#'   name = "test_table",
#'   overwrite = TRUE
#' )
#' adherenceSlidingWindow(cdm, name = "test_table", cma = c("CMA1", "CMA2", "CMA3", "CMA4"))
#' }
adherenceSlidingWindow <- function(cdm = NULL,
                                   data = NULL,
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
                                   name = NULL,
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

  df <- loadData(cdm, name, data, delayObservationWindowStart)

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

  if (length(computedCMAs)>0){
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

#' For using adherence calculations function with large datasets.
#'
#' Calculates adherence in batches
#'
#' @param reference reference to table
#' @param cdm a cdm_reference object with 'name' table
#' @param ... input values for adherenceCalculations
#' @param batchSize default 1000.
#' @param cma By default the function calculates adherence with all different CMA algorithms (CMA1-CMA9).
#' @param resultsTableName name of the table where results are stores
#'
#' @export
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#'
#' chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)
#'
#' CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure, cdm = cdm, cma = "CMA5")
#' }
#'
adherenceCalculationsLoader <- function(reference,
                                        cdm,
                                        resultsTableName = "adherenceFromOMOP_results",
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
                                        batchSize = 1000,
                                        ...) {
  if (is.null(reference)) {
    cli::cli_alert_warning("Provided reference is faulty")
    return(NULL)
  }
  uniqueSubjectIds <- reference %>%
    dplyr::distinct(person_id) %>%
    dplyr::pull(person_id)

  batches <- split(uniqueSubjectIds, ceiling(seq_along(uniqueSubjectIds) / batchSize))

  writeSchema <- CDMConnector::cdmWriteSchema(cdm)
  nameWithPrefix <- paste0(writeSchema[[2]], resultsTableName)
  connection <- CDMConnector::cdmCon(cdm)

  if (nameWithPrefix %in% DBI::dbListTables(conn = connection, name = DBI::Id(schema = writeSchema[[1]], table = nameWithPrefix))){
    cli::cli_alert_info(paste("Table", nameWithPrefix, "already exists!"))
    return( NULL )
  }

  for (i in seq_along(batches)) {
    cli::cli_alert_info(paste("Working with batch", names(batches)[i], "out of", length(batches)))

    batch <- batches[[i]]
    data <- reference %>%
      dplyr::filter(person_id %in% batch) %>%
      dplyr::collect()

    result <- adherenceCalculations(
      cdm = NULL,
      data = data,
      cma = cma,
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

  newTable <- dplyr::tbl(connection, CDMConnector::inSchema(CDMConnector::cdmWriteSchema(cdm), table = resultsTableName, CDMConnector::dbms(connection)))

  return(newTable)
}

#' Calculating simple CMAs for the duration on observation period
#'
#' @param cdm a cdm_reference object with 'name' table
#' @param name name of the table used for adherence calculations
#' @param ... input values for adhereR CMA function call
#' @param cma By default the function calculates adherence with all different CMA algorithms (CMA1-CMA9).
#' @param data instead of cdm provide in-memory data.
#' @param medicationGroup CMA is calculated for each defined group. Medication group is a named list with a name and drug_concept_id values. For creating this list it is highly recommended to use CodelistGenerator \link[CodelistGenerator]{"https://darwin-eu.github.io/CodelistGenerator/articles/a04_GenerateVocabularyBasedCodelist.html"}
#' @param delayObservationWindowStart delays adherence calculation window until first medication purchase
#' @param cleanRows If TRUE, rows with NA CMA values are dropped and window.ID is reset to be sequential within each contiguous non-NA CMA interval.
#'
#'
#' @returns table with CMA results
#' @export
#'
#' @examples
#' \dontrun{
#' connection <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomiaDir()))
#'
#' cdm <- CDMConnector::cdmFromCon(
#'   con = connection,
#'   cdmSchema = "main",
#'   writeSchema = "main",
#'   cdmName = "my_duckdb_database"
#' )
#'
#' cdm <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   ingredients = c("acetaminophen", "metformin", "simvastatin"),
#'   name = "test_table",
#'   overwrite = TRUE
#' )
#' adherenceCalculations(
#'   cdm = cdm,
#'   ingredient = c("acetaminophen"),
#'   name = "test_table",
#'   cma = "CMA1"
#' )
#' }
adherenceCalculations <- function(cdm = NULL,
                                  data = NULL,
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
                                  name = NULL,
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

  df <- loadData(cdm, name, data, delayObservationWindowStart)
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

  if (cleanRows){
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
#' @returns Data frame combining CMA results with patient information. For sliding window
#'   results, each window row is matched with patient info from drug exposures within
#'   that window. The days_to_death column will be NA for windows without drug exposures.
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
cleanNARows <- function(data){
  if ("window.ID" %in% colnames(data)){
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
