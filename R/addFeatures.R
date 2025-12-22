#' adds bmi values to CMA values
#'
#' @param cdm a cdm_reference object
#' @param cma_table reference to a table in database or one in memory that contains cma values
#'
#' @returns
#' @export
#'
#' @examples
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
        days_to_death,
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


#' Adds cohort columns to cma data
#'
#' @param cohortTable cohort table that is used for adding features. reference or in memory table with columns cohort_definition_id subject_id cohort_start_date cohort_end_date
#' @param cohortIdMappingToNames
#' @param cmaTable reference to cma table
#'
#' @returns reference to table with cma and cohort data
#' @export
#'
#' @examples
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


#' Counts people, returns an overview of subjects
#'
#' @param cdm A cdm reference via CDMConnector.
#' @param cmaTable reference or in memory dataframe that contains CMA values
#' @param generatedTable reference or in memory dataframe that contains generateChronicDrug exposure output
#' @param cohortSchema string value of a schema in database
#' @param cohortId vector with cohort ids to count people included
#' @param n minimum number of rows per year for a person to be included in 'Yearly drug exposure record count' table
#'
#' @returns list
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#' chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)
#' CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure, cdm = cdm, cma = "CMA5")
#' counts <- countPeople(cdm = cdm, cmaTable = CMA_values, generatedTable = chronicDrugExposure)
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
