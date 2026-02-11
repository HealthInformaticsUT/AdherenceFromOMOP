#' Get drug exposure entries for adherence calculations
#'
#' @inheritParams cdmDoc
#' @inheritParams nameDoc
#' @inheritParams conceptSetDoc
#' @inheritParams indexDateDoc
#'
#' @param overwrite If table with provided name exists, should it be overwritten?
#'   Default: `FALSE`.
#' @param subjects (`numeric` or `NULL`) Optional vector of person_id values to restrict
#'   the drug exposure data to specific subjects.
#' @param cohort (`tbl` or `NULL`) Optional cohort table reference to filter drug exposures.
#'   Only drug exposures for subjects in the cohort and within the cohort period will be included.
#' @param cohortId (`numeric` or `NULL`) If `cohort` is provided, optional vector of
#'   cohort_definition_id values to use. If `NULL`, all cohorts in the table are used.
#' @param ... Additional arguments (currently unused).
#'
#' @return A database table reference containing drug exposure records with patient information.
#'   Columns include: person_id, drug_concept_id, drug_exposure_start_date, days_supply,
#'   date_of_birth, sex, date_of_death, observation_period_start_date, observation_period_end_date.
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
#'   name = "chronic_drug_exposure",
#'   overwrite = TRUE
#' )
#' }
generateChronicDrugExposure <- function(cdm,
                                        name = "adherenceFromOMOP",
                                        conceptSet = NULL,
                                        indexDate = "drug_exposure_start_date",
                                        overwrite = FALSE,
                                        subjects = NULL,
                                        cohort = NULL,
                                        cohortId = NULL,
                                        ...) {
  if (name %in% (!name %in% CDMConnector::listTables(CDMConnector::cdmCon(cdm), schema = CDMConnector::cdmWriteSchema(cdm))) &
    !overwrite) {
    cli::cli_alert_warning("Already has table named { name }. Set overwrite = TRUE to write over specified table.")
    return(NULL)
  }


  if (is.null(conceptSet)) {
    cli::cli_alert_info("Drug concept ids not provided, all drugs included.")

    data <- cdm$drug_exposure
  } else {
    cli::cli_alert_info("Using the drug concept id list")

    conceptIds <- unlist(conceptSet, use.names = FALSE)
    data <- dplyr::filter(cdm$drug_exposure, drug_concept_id %in% conceptIds)
  }

  cli::cli_alert_info("Using Patient Profiles package to add patient data.")

  data <- data %>%
    dplyr::left_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::inner_join(cdm$observation_period, by = c("person_id")) %>%
    PatientProfiles::addDateOfBirth() %>%
    PatientProfiles::addSex() %>%
    PatientProfiles::addDeathDate(indexDate = indexDate) %>%
    dplyr::select(
      person_id,
      drug_concept_id,
      # concept_name,
      drug_exposure_start_date,
      days_supply,
      # visit_occurrence_id,
      # route_concept_id,
      # route_source_value,
      date_of_birth,
      sex,
      date_of_death,
      observation_period_start_date,
      observation_period_end_date
    ) %>%
    dplyr::compute(
      name = name,
      temporary = FALSE,
      schema = CDMConnector::cdmWriteSchema(cdm),
      overwrite = TRUE
    )

  if (!is.null(cohort)) {
    cohortFiltered <- cohort

    if (!is.null(cohortId)) {
      cohortFiltered <- cohortFiltered %>%
        dplyr::filter(cohort_definition_id %in% cohortId)
    }

    if (nrow(dplyr::collect(cohortFiltered, n = 1)) < 1) {
      cli::cli_alert_warning("Cohort is empty after filtering. No records to process.")
      return(NULL)
    }

    data <- data %>%
      dplyr::inner_join(cohortFiltered, dplyr::join_by(x$person_id == y$subject_id)) %>%
      dplyr::filter(
        drug_exposure_start_date >= cohort_start_date &
          drug_exposure_start_date <= cohort_end_date
      )
  }

  # important that days supply values exceed 0
  daysSupply0 <- data %>%
    dplyr::filter(days_supply <= 0) %>%
    dplyr::count() %>%
    dplyr::pull(n)

  if (daysSupply0 > 0) {
    cli::cli_alert_info("Days supply value must be atleast 1.")
  }

  # looking at a specific set of subjects
  if (!is.null(subjects)) {
    cli::cli_alert_info("Filtering subjects { subjects }.")

    numberOfEvents <- data %>%
      dplyr::count() %>%
      dplyr::pull(n)
    cli::cli_alert("Number of events before filtering { numberOfEvents }.")

    filteredData <- data %>% dplyr::filter(person_id %in% subjects)
    numberOfEvents <- filteredData %>%
      dplyr::count() %>%
      dplyr::pull(n)
    cli::cli_alert("Number of events after filtering { numberOfEvents }.")

    if (numberOfEvents < 1) {
      cli::cli_alert_warning("No events remain after filtering.")
      return(NULL)
    }

    data <- filteredData
  }

  chronicDrugExposure <- data %>%
    dplyr::compute(
      name = name,
      temporary = FALSE,
      schema = CDMConnector::cdmWriteSchema(cdm),
      overwrite = TRUE
    )

  return(chronicDrugExposure)
}
