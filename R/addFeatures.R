#' adds bmi values to CMA values
#'
#' @inheritParams adherenceDataDoc
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @inheritParams temporaryDoc
#' @inheritParams overwriteDoc
#'
#' @returns Reference to database table with adherence data and BMI category
#'   information added.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   name = "chronic_drug_exposure_table",
#'   overwrite = T
#' )
#' CMA_values <- calculateAdherenceBatched(
#'   drugExposure = chronicDrugExposure,
#'   cdm = cdm, cma = "CMA5"
#' )
#' withBMI <- addBMI(CMA_values, cdm)
#' }
addBMI <- function(adherenceData,
                   cdm,
                   name = "addBMI_results",
                   temporary = TRUE,
                   overwrite = FALSE) {
  cma_table <- adherenceData

  bmi <- cdm$measurement %>%
    dplyr::semi_join(cma_table, by = "person_id") %>%
    dplyr::filter(measurement_concept_id == 4245997) %>%
    dplyr::select(
      person_id,
      measurement_date,
      unit_source_value,
      value_source_value
    )

  # Whether the sliding-window columns exist has to be checked on the table
  has_windows <- all(c("window.start", "window.end") %in% colnames(cma_table))

  if (has_windows) {
    by <- dplyr::join_by(
      person_id,
      dplyr::between(
        y$measurement_date,
        x$window.start.year.before,
        x$window.start
      )
    )

    cma_table <- cma_table %>%
      dplyr::mutate(window.start.year.before = window.start - 365L) %>%
      dplyr::left_join(bmi, by)

    rows_without_bmi <- cma_table %>%
      dplyr::filter(is.na(value_source_value)) %>%
      dplyr::select(-measurement_date, -unit_source_value, -value_source_value)

    closest_bmi <- rows_without_bmi %>%
      dplyr::left_join(bmi, by = "person_id") %>%
      dplyr::mutate(date_diff = abs(dplyr::sql('(measurement_date - "window.start")'))) %>%
      dplyr::group_by(name, person_id, window.start, window.end, group) %>%
      dbplyr::window_order(date_diff) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-date_diff)

    cma_table <- dplyr::union_all(
      cma_table %>% dplyr::filter(!is.na(value_source_value)),
      closest_bmi
    )

    cma_table <- cma_table %>%
      dplyr::mutate(
        value_source_value_num = as.numeric(value_source_value),
        bmi_category = dplyr::case_when(
          value_source_value_num < 25.0 ~ "Under or normal weight",
          value_source_value_num >= 25.0 &
            value_source_value_num < 30.0 ~ "Overweight",
          value_source_value_num >= 30.0 ~ "Obese",
          is.na(value_source_value_num) ~ "Unknown"
        )
      ) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::select(
        name,
        person_id,
        window.id,
        window.start,
        window.end,
        cma,
        group,
        date_of_birth,
        sex,
        date_of_death,
        bmi_category,
        measurement_date
      ) %>%
      dplyr::distinct()
  } else {
    avg_bmi <- bmi %>%
      dplyr::group_by(person_id) %>%
      dplyr::summarise(avg_bmi = mean(as.numeric(value_source_value), na.rm = TRUE))

    cma_table <- cma_table %>%
      dplyr::left_join(avg_bmi, dplyr::join_by(person_id)) %>%
      dplyr::mutate(
        bmi_category = dplyr::case_when(
          avg_bmi < 25.0 ~ "Under or normal weight",
          avg_bmi >= 25.0 &
            avg_bmi < 30.0 ~ "Overweight",
          avg_bmi >= 30.0 ~ "Obese",
          is.na(avg_bmi) ~ "Unknown"
        )
      )
  }

  if (temporary) {
    cma_table <- cma_table %>%
      dplyr::compute(name = name, temporary = TRUE, overwrite = overwrite)
  } else {
    cma_table <- cma_table %>%
      dplyr::compute(
        name = name,
        temporary = FALSE,
        schema = CDMConnector::cdmWriteSchema(cdm),
        overwrite = overwrite
      )
  }

  return(cma_table)
}


#' Adds cohort columns to cma data
#'
#' @inheritParams adherenceDataDoc
#' @param cohort Cohort table reference:
#'   cohort_definition_id, person_id, cohort_start_date, cohort_end_date.
#' @param cohortIdMappingToNames (`list` or `NULL`)
#'   Optional mapping of cohort IDs to names.
#' @param cdm (`cdm_reference` or `NULL`) A CDM reference object created with
#'   CDMConnector. Required when `temporary = FALSE`, so the result can be
#'   written as a permanent table in the CDM write schema.
#' @inheritParams nameDoc
#' @inheritParams temporaryDoc
#' @inheritParams overwriteDoc
#'
#' @returns Reference to database table with adherence and cohort data.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#'
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   name = "chronic_drug_exposure_table",
#'   overwrite = T
#' )
#'
#' CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")
#'
#' observ_dates <- cdm$observation_period %>%
#'   dplyr::filter(person_id == 1) %>%
#'   dplyr::select(observation_period_start_date, observation_period_end_date) %>%
#'   dplyr::collect()
#'
#' cohortTable <- dplyr::tibble(
#'   cohort_definition_id = 1,
#'   person_id = 1,
#'   cohort_start_date = observ_dates$observation_period_start_date,
#'   cohort_end_date = observ_dates$observation_period_end_date
#' )
#'
#' withCohort <- addCohorts(CMA_values, cohortTable)
#' }
addCohorts <- function(adherenceData,
                       cohort,
                       cohortIdMappingToNames = NULL,
                       cdm = NULL,
                       name = "addCohorts_results",
                       temporary = TRUE,
                       overwrite = FALSE) {
  if (!temporary && is.null(cdm)) {
    cli::cli_alert_warning("cdm must be provided when temporary = FALSE. Writing as a temporary table instead.")
    temporary <- TRUE
  }

  cmaTable <- adherenceData %>% dplyr::rename_with(tolower)

  # Unify both cases up front so there is a single join path
  if (!all(c("window.start", "window.end") %in% colnames(cmaTable))) {
    cmaTable <- cmaTable %>%
      dplyr::mutate(
        window.id    = 1L,
        window.start = observation_window_start,
        window.end   = observation_period_end_date
      )
  }

  # One row per key in cmaTable — adjust if your grain differs
  keyCols <- c("person_id", "name", "group", "window.id")

  uniqueCohorts <- cohort %>%
    dplyr::distinct(cohort_definition_id) %>%
    dplyr::pull(cohort_definition_id) %>%
    sort()

  if (is.null(cohortIdMappingToNames)) {
    cohortIdMappingToNames <- list(cohort_name = uniqueCohorts,
                                   cohort_definition_id = uniqueCohorts)
  }
  cohortNames <- cohortIdMappingToNames$cohort_name[
    match(uniqueCohorts, cohortIdMappingToNames$cohort_definition_id)
  ]
  cohortNames <- ifelse(is.na(cohortNames), uniqueCohorts, cohortNames)

  indCols  <- paste0("cohort_", cohortNames)
  timeCols <- paste0("time_in_", cohortNames)

  # 1. Interval join on person + overlap, only the needed columns
  overlapRows <- cmaTable %>%
    dplyr::select(dplyr::all_of(keyCols), window.start, window.end) %>%
    dplyr::inner_join(
      cohort %>%
        dplyr::select(cohort_definition_id, subject_id,
                      cohort_start_date, cohort_end_date),
      by = dplyr::join_by(
        person_id == subject_id,
        window.start <= cohort_end_date,
        window.end   >= cohort_start_date
      )
    ) %>%
    dplyr::mutate(
      overlap_start = dplyr::if_else(cohort_start_date > window.start,
                                     cohort_start_date, window.start),
      overlap_end   = dplyr::if_else(cohort_end_date < window.end,
                                     cohort_end_date, window.end)
    ) %>%
    dplyr::mutate(days = !!CDMConnector::datediff("overlap_start", "overlap_end")) %>%
    dplyr::mutate(days = days + 1L)

  # 2. Single aggregation straight to wide format
  aggExprs <- c(
    rlang::set_names(lapply(uniqueCohorts, function(id) {
      rlang::expr(max(dplyr::if_else(cohort_definition_id == !!id, 1L, 0L), na.rm = TRUE))
    }), indCols),
    rlang::set_names(lapply(uniqueCohorts, function(id) {
      rlang::expr(sum(dplyr::if_else(cohort_definition_id == !!id, days, 0L), na.rm = TRUE))
    }), timeCols)
  )

  perWindow <- overlapRows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keyCols))) %>%
    dplyr::summarise(!!!aggExprs, .groups = "drop")

  # 3. Attach to all CMA rows; non-members get 0
  result <- cmaTable %>%
    dplyr::select(dplyr::all_of(keyCols), window.start, window.end, cma) %>%
    dplyr::left_join(perWindow, by = keyCols) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(indCols, timeCols)),
                                ~ dplyr::coalesce(.x, 0L)))

  result <- if (temporary) {
    dplyr::compute(result, name = name, temporary = TRUE, overwrite = overwrite)
  } else {
    dplyr::compute(result, name = name, temporary = FALSE,
                   schema = CDMConnector::cdmWriteSchema(cdm),
                   overwrite = overwrite)
  }

  # 4. Cheap check on the materialised (small) table
  nMatched <- result %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(indCols), ~ .x > 0)) %>%
    dplyr::count() %>%
    dplyr::pull()

  if (nMatched == 0) {
    cli::cli_alert_info("Cohort time periods and time periods CMA is calculated for don't match.")
  }

  result
}


#' Summarise patient counts
#'
#' Returns summary statistics about the number of subjects in adherence data
#' and drug exposure data.
#'
#' @inheritParams adherenceDataDoc
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @param n (`numeric(1)`) Minimum number of rows per year for a person
#'  to be included
#'   in 'Yearly drug exposure record count' table. Default: 1.
#'
#' @returns Named list with patient count summaries.
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
#' adherenceData <- calculateAdherenceBatched(drugExposure, cdm, cma = "CMA5")
#'
#' counts <- summarisePatientCounts(
#'   adherenceData = adherenceData,
#'   drugExposure = drugExposure,
#'   cdm = cdm
#' )
#' }
summarisePatientCounts <- function(adherenceData = NULL,
                                   drugExposure = NULL,
                                   cdm = NULL,
                                   n = 1) {
  result <- list()

  if (!is.null(adherenceData)) {
    count <- adherenceData %>%
      dplyr::select(person_id) %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()
    result["Number of people CMA generated for"] <- as.integer(count)
  }
  if (!is.null(drugExposure)) {
    count <- drugExposure %>%
      dplyr::select(person_id) %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()
    result["Number of people included"] <- as.integer(count)
  }
  if (!is.null(drugExposure)) {
    count_n_for_time_t <- drugExposure %>%
      dplyr::mutate(year = lubridate::year(drug_exposure_start_date)) %>%
      dplyr::group_by(year, person_id) %>%
      dplyr::summarise(purchase_count = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(purchase_count >= n) %>%
      dplyr::count(year, name = "n_persons") %>%
      dplyr::arrange(year) %>%
      dplyr::collect()

    result["Yearly drug exposure record count"] <- list(count_n_for_time_t)
  }

  return(result)
}
