#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom data.table %like%
#' @importFrom stats setNames
#' @importFrom dplyr all_of between overlaps
## usethis namespace: end
utils::globalVariables(c(
  ".", ".data", "x", "y",
  "cohort_definition_id", "cohort_start_date", "cohort_end_date", "cohort_name",
  "overlap_start", "overlap_end", "time_in_cohort", "subject_id",
  "window.start", "window.end", "window.id", "name", "cma",
  "drug_concept_id", "route_source_value", "route_concept_id",
  "visit_occurrence_id", "days_supply", "concept_name", "person_id", "cdm",
  "measurement_concept_id", "measurement_date", "unit_source_value",
  "value_source_value", "group", "date_of_birth", "sex", "days_to_death",
  "bmi_category", "drug_exposure_start_date", "observation_period_duration",
  "concept_class_id", "observation_period_start_date",
  "observation_period_end_date", "year", "n", "has_min_purchases",
  "purchase_count", "CMA", "nonNASeries", "window.ID"
))
