#' Add BMI category to CMA results
#'
#' Enriches CMA calculation results with Body Mass Index (BMI) categories derived
#' from measurement records in the OMOP CDM. For sliding window calculations, BMI
#' is matched to measurements taken within one year before each window start date.
#' For simple CMA calculations, the average BMI across all measurements is used.
#' BMI concept_id 4245997 is used to identify BMI measurements.
#'
#' @param cma_table Data frame or database table reference containing CMA results.
#'   Must include person_id column. For sliding window results, should also contain
#'   window.start and window.end columns.
#' @param cdm A cdm_reference object created by CDMConnector, providing access to
#'   the measurement table containing BMI records.
#'
#' @returns Data frame with original CMA columns plus bmi_category column containing
#'   one of: "Under or normal weight" (BMI < 25), "Overweight" (25 <= BMI < 30),
#'   "Obese" (BMI >= 30), or "Unknown" (no BMI measurement available).
#'   For sliding window results, also includes measurement_date.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#' cma_results <- adherenceSlidingWindow(cdm, name = "drug_exposure", cma = "CMA5")
#' cma_with_bmi <- addBMI(cma_results, cdm)
#' }
addBMI <- function(cma_table, cdm) {
  cma_table <- dplyr::collect(cma_table)

  person_ids <- cma_table %>%
    dplyr::distinct(person_id) %>%
    dplyr::pull(person_id)

  bmi <- cdm$measurement %>%
    dplyr::filter(person_id %in% person_ids) %>%
    dplyr::filter(measurement_concept_id == 4245997) %>%
    dplyr::select(
      person_id,
      measurement_date,
      unit_source_value,
      value_source_value
    ) %>%
    dplyr::collect()

  if (all(c("window.start", "window.end") %in% colnames(cma_table))) {
    by <- dplyr::join_by(
      person_id,
      between(
        y$measurement_date,
        x$window.start,
        x$window.start.year.before
      )
    )
    cma_table <- cma_table %>%
      dplyr::mutate(window.start.year.before = as.Date(window.start - 365)) %>%
      dplyr::left_join(bmi, by, copy = TRUE) %>%
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
      dplyr::summarise(avg_bmi = mean(as.numeric(value_source_value)))

    cma_table <- cma_table %>%
      dplyr::left_join(avg_bmi, dplyr::join_by(person_id), copy = TRUE) %>%
      dplyr::mutate(
        BMIcategory = dplyr::case_when(
          avg_bmi < 25.0 ~ "Under or normal weight",
          avg_bmi >= 25.0 &
            avg_bmi < 30.0 ~ "Overweight",
          avg_bmi >= 30.0 ~ "Obese",
          is.na(avg_bmi) ~ "Unknown"
        )
      )
  }

  return(cma_table)
}


#' Add cohort membership indicators to CMA results
#'
#' Enriches CMA calculation results with cohort membership information by joining
#' CMA data with cohort tables. For each cohort, adds binary indicator columns
#' (whether patient was in cohort during the observation window) and time-in-cohort
#' columns (number of days the patient spent in each cohort during the window).
#' Handles both sliding window and simple CMA results.
#'
#' @param cmaTable Data frame or database table reference containing CMA results.
#'   Must include person_id column. For sliding window results, should contain
#'   window.start and window.end columns; for simple CMA, should contain
#'   observation_window_start and observation_period_end_date.
#' @param cohortTable Data frame or database table reference containing cohort data
#'   with columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date.
#' @param cohortIdMappingToNames Optional named list for mapping cohort IDs to readable
#'   names in output columns. Should contain elements "cohort_name" and "cohort_definition_id"
#'   as parallel vectors. If NULL, cohort IDs are used as column name suffixes.
#'
#' @returns Data frame with original CMA columns plus, for each cohort: a binary
#'   indicator column (cohort_{name}) and a time-in-cohort column (time_in_{name})
#'   showing days of overlap between cohort period and CMA observation window.
#'   Returns NULL if cohort periods do not overlap with CMA calculation periods.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#' cma_results <- adherenceCalculations(cdm, name = "drug_exposure", cma = "CMA5")
#' cohort_mapping <- list(cohort_name = c("diabetes", "hypertension"),
#'                        cohort_definition_id = c(1, 2))
#' cma_with_cohorts <- addCohorts(cma_results, cdm$cohort, cohort_mapping)
#' }
addCohorts <- function(cmaTable, cohortTable, cohortIdMappingToNames = NULL) {
  cmaTable <- dplyr::collect(cmaTable)
  cohort <- dplyr::collect(cohortTable)

  uniqueCohorts <- unique(cohort$cohort_definition_id)

  if (is.null(cohortIdMappingToNames)) {
    cohortIdMappingToNames <- list()
    cohortIdMappingToNames["cohort_name"] <- uniqueCohorts
    cohortIdMappingToNames["cohort_definition_id"] <- uniqueCohorts
  }

  features <- c(
    paste0("cohort_definition_id_", uniqueCohorts),
    paste0("time_in_cohort_", uniqueCohorts)
  )

  # joining cma and cohort information differ whether or not sliding window was used
  if (all(c("window.start", "window.end") %in% colnames(cmaTable))) {
    cma_with_cohort <- cohort %>%
      dplyr::mutate(cohort_name = cohort_definition_id) %>%
      dplyr::right_join(
        cmaTable,
        dplyr::join_by(
          y$person_id == x$subject_id,
          dplyr::overlaps(
            x$cohort_start_date,
            x$cohort_end_date,
            y$window.start,
            y$window.end
          )
        )
      ) %>%
      dplyr::mutate(
        overlap_start = pmax(cohort_start_date, window.start),
        overlap_end = pmin(cohort_end_date, window.end),
        time_in_cohort = as.integer(pmax(
          0, overlap_end - overlap_start + 1
        ))
      )
  } else {
    cma_with_cohort <- cohort %>%
      dplyr::mutate(cohort_name = cohort_definition_id) %>%
      dplyr::right_join(
        cmaTable,
        dplyr::join_by(
          y$person_id == x$subject_id,
          dplyr::overlaps(
            x$cohort_start_date,
            x$cohort_end_date,
            y$observation_window_start,
            y$observation_period_end_date
          )
        )
      ) %>%
      dplyr::mutate(
        overlap_start = pmax(cohort_start_date, observation_window_start),
        overlap_end = pmin(cohort_end_date, observation_period_end_date),
        time_in_cohort = as.integer(pmax(
          0, overlap_end - overlap_start + 1
        ))
      ) %>%
      dplyr::mutate(
        window.id = 1,
        window.start = observation_window_start,
        window.end = observation_period_end_date,
      )
  }

  if (all(is.na(unique(cma_with_cohort$cohort_definition_id)))){
    cli::cli_alert_info("Cohort time periods and time periods CMA is calculated for don't match.")
    return( NULL )
  }

  cohort_names <- stats::setNames(c(
    paste0("cohort_definition_id_", cohortIdMappingToNames$cohort_definition_id),
    paste0("time_in_cohort_", cohortIdMappingToNames$cohort_definition_id)
  ), c(
    paste0("cohort_", cohortIdMappingToNames$cohort_name),
    paste0("time_in_", cohortIdMappingToNames$cohort_name)
  ))

  cma_table_with_data <- cma_with_cohort %>%
    tidyr::pivot_wider(
      names_from = cohort_name,
      values_from = c(cohort_definition_id, time_in_cohort),
      values_fill = 0,
      names_glue = "{.value}_{cohort_name}"
    ) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::group_by(
      subject_id,
      name,
      window.id,
      window.start,
      window.end,
      cma,
      group
    ) %>%
    dplyr::summarise(dplyr::across(features, sum)) %>%
    dplyr::mutate(dplyr::across(features[1:length(features) / 2], ~ ifelse(. > 0, 1, 0))) %>%
    dplyr::rename(all_of(cohort_names)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(subject_id)

  return(cma_table_with_data)
}


#' Generate summary counts of patients in adherence analysis
#'
#' Provides an overview of patient counts at different stages of the adherence
#' analysis pipeline. Counts unique patients in CMA results, drug exposure data,
#' and optionally in specified cohorts. Also generates yearly breakdowns of patients
#' with drug exposure records meeting a minimum threshold.
#'
#' @param cdm A cdm_reference object created by CDMConnector. Required only when
#'   cohortSchema and cohortId are provided to count cohort membership.
#' @param cmaTable Optional data frame or database table reference containing CMA
#'   calculation results. If provided, counts unique person_id values.
#' @param generatedTable Optional data frame or database table reference containing
#'   output from generateChronicDrugExposure. If provided, counts unique patients
#'   and generates yearly drug exposure summaries.
#' @param cohortSchema Optional character string specifying the database schema
#'   containing a cohort table. Used with cohortId to count patients in cohorts.
#' @param cohortId Optional numeric vector of cohort_definition_id values to count.
#'   Used together with cohortSchema.
#' @param n Integer specifying the minimum number of drug exposure records per year
#'   for a patient to be included in the yearly summary. Default is 1.
#'
#' @returns Named list containing:
#'   \describe{
#'     \item{"Number of people CMA generated for"}{Count from cmaTable if provided}
#'     \item{"Number of people included"}{Count from generatedTable if provided}
#'     \item{"Yearly drug exposure record count"}{Data frame with year and n_persons
#'       columns showing patients meeting minimum record threshold per year}
#'     \item{"Number of people in cohort"}{Count from cohort if cohortSchema/cohortId provided}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm, name = "chronic_drug_exposure_table", overwrite = TRUE
#' )
#' CMA_values <- adherenceCalculationsLoader(
#'   reference = chronicDrugExposure, cdm = cdm, cma = "CMA5"
#' )
#' counts <- countPeople(
#'   cdm = cdm, cmaTable = CMA_values, generatedTable = chronicDrugExposure
#' )
#' }
countPeople <- function(cdm = NULL,
                        cmaTable = NULL,
                        generatedTable = NULL,
                        cohortSchema = NULL,
                        cohortId = NULL,
                        n = 1) {
  result <- list()

  if (!is.null(cmaTable)) {
    count <- cmaTable %>%
      dplyr::select(person_id) %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()
    result["Number of people CMA generated for"] <- as.integer(count)
  }
  if (!is.null(generatedTable)){
    count <- generatedTable %>%
      dplyr::select(person_id) %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()
    result["Number of people included"] <- as.integer(count)
  }
  if (!is.null(generatedTable)) {
    count_n_for_time_t <- generatedTable %>%
      dplyr::collect() %>%
      dplyr::group_by(person_id) %>%
      dplyr::mutate(year = lubridate::year(drug_exposure_start_date)) %>%
      dplyr::group_by(year, person_id) %>%
      dplyr::summarise(purchase_count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(has_min_purchases = purchase_count >= n) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        n_persons = sum(has_min_purchases),
        .groups = "drop"
      ) %>%
      dplyr::distinct()

    result["Yearly drug exposure record count"] <- list(count_n_for_time_t)
  }

  if (!is.null(cohortSchema) & !is.null(cohortId) & !is.null(cdm)) {
    connection <- CDMConnector::cdmCon(cdm)
    cohort <- dplyr::tbl(
      connection,
      CDMConnector::inSchema(cohortSchema, "cohort")
    ) %>%
      dplyr::filter(cohort_definition_id %in% cohortId)

    count <- cohort %>%
      dplyr::select(subject_id) %>%
      dplyr::distinct() %>%
      dplyr::count() %>%
      dplyr::pull()
    result["Number of people in cohort"] <- as.integer(count)
  }
  return(result)
}
