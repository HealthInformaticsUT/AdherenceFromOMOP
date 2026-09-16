#' adds bmi values to CMA values
#'
#' @inheritParams adherenceDataDoc
#' @inheritParams cdmDoc
#'
#' @returns Adherence data with BMI category information added.
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
addBMI <- function(adherenceData, cdm) {
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
      dplyr::mutate(window.start.year.before = window.start - 365) %>%
      dplyr::left_join(bmi, by)


    rows_without_bmi <- cma_table %>%
      dplyr::filter(is.na(value_source_value)) %>%
      dplyr::select(-measurement_date, -unit_source_value, -value_source_value)

    closest_bmi <- rows_without_bmi %>%
      dplyr::left_join(bmi, by = "person_id") %>%
      dplyr::mutate(
        date_diff = abs(
          dplyr::sql("EXTRACT(DAY FROM (measurement_date - window.start))")
        )
      ) %>%
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

  return(cma_table)
}


#' Adds cohort columns to cma data
#'
#' @inheritParams adherenceDataDoc
#' @param cohort Cohort table reference:
#'   cohort_definition_id, person_id, cohort_start_date, cohort_end_date.
#' @param cohortIdMappingToNames (`list` or `NULL`)
#'   Optional mapping of cohort IDs to names.
#'
#' @returns Reference to table with adherence and cohort data.
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
addCohorts <- function(adherenceData, cohort, cohortIdMappingToNames = NULL) {

  cmaTable <- adherenceData %>% dplyr::rename_with(tolower)

  uniqueCohorts <- cohort %>%
    dplyr::distinct(cohort_definition_id) %>%
    dplyr::pull(cohort_definition_id)

  if (is.null(cohortIdMappingToNames)) {
    cohortIdMappingToNames <- list()
    cohortIdMappingToNames["cohort_name"] <- uniqueCohorts
    cohortIdMappingToNames["cohort_definition_id"] <- uniqueCohorts
  }

  features <- c(
    paste0("cohort_definition_id_", uniqueCohorts),
    paste0("time_in_cohort_", uniqueCohorts)
  )

  has_windows <- all(c("window.start", "window.end") %in% colnames(cmaTable))

  # joining cma and cohort information differ whether or not sliding window was used
  if (has_windows) {
    cma_with_cohort <- cohort %>%
      dplyr::mutate(cohort_name = cohort_definition_id) %>%
      dplyr::right_join(
        cmaTable,
        dplyr::join_by(
          overlaps(
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
          0, overlap_end - overlap_start
        ))
      )
  } else {
    cma_with_cohort <- cohort %>%
      dplyr::mutate(cohort_name = cohort_definition_id) %>%
      dplyr::right_join(
        cmaTable,
        dplyr::join_by(
          overlaps(
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
          0, overlap_end - overlap_start
        ))
      ) %>%
      dplyr::mutate(
        window.id = 1,
        window.start = observation_window_start,
        window.end = observation_period_end_date,
      )
  }

  # Aggregate down to (subject x window x cohort)
  cma_with_cohort <- cma_with_cohort %>%
    dplyr::group_by(
      person_id,
      name,
      window.id,
      window.start,
      window.end,
      cma,
      group,
      cohort_name,
      cohort_definition_id
    ) %>%
    dplyr::summarise(
      time_in_cohort = sum(time_in_cohort, na.rm = TRUE),
      .groups = "drop"
    )

  # Single-value check ("did anything match?"), not a collect of the data.
  matched_rows <- cma_with_cohort %>%
    dplyr::filter(!is.na(cohort_definition_id)) %>%
    dplyr::count() %>%
    dplyr::pull()

  if (matched_rows == 0) {
    cli::cli_alert_info("Cohort time periods and time periods CMA is calculated for don't match.")
    return(NULL)
  }

  cohort_names <- stats::setNames(c(
    paste0("cohort_definition_id_", cohortIdMappingToNames$cohort_definition_id),
    paste0("time_in_cohort_", cohortIdMappingToNames$cohort_definition_id)
  ), c(
    paste0("cohort_", cohortIdMappingToNames$cohort_name),
    paste0("time_in_", cohortIdMappingToNames$cohort_name)
  ))

  # Build one indicator + one time-in-cohort column per cohort id
  # Each row's own cohort_definition_id determines which pair of columns
  # gets a non-zero value; the rest are 0, so summing per (subject, window,
  # group) below reproduces what the wide pivot + sum used to do.
  indicator_exprs <- rlang::set_names(
    lapply(uniqueCohorts, function(id) {
      rlang::expr(dplyr::if_else(cohort_definition_id == !!id, 1L, 0L, missing = 0L))
    }),
    paste0("cohort_definition_id_", uniqueCohorts)
  )
  time_in_cohort_exprs <- rlang::set_names(
    lapply(uniqueCohorts, function(id) {
      rlang::expr(dplyr::if_else(cohort_definition_id == !!id, time_in_cohort, 0, missing = 0))
    }),
    paste0("time_in_cohort_", uniqueCohorts)
  )

  cma_table_with_data <- cma_with_cohort %>%
    dplyr::mutate(!!!indicator_exprs, !!!time_in_cohort_exprs) %>%
    dplyr::group_by(
      person_id,
      name,
      window.id,
      window.start,
      window.end,
      cma,
      group
    ) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(features), ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(features[1:(length(features) / 2)]),
      ~ ifelse(. > 0, 1, 0)
    )) %>%
    dplyr::rename(dplyr::all_of(cohort_names)) %>%
    dplyr::arrange(person_id)

  return(cma_table_with_data)
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
