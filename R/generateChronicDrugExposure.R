#' Generate drug exposure data for adherence calculations
#'
#' Extracts and prepares drug exposure records from an OMOP CDM database for use
#' in medication adherence calculations. Joins drug exposure data with patient
#' demographics (date of birth, sex, days to death) and observation period information.
#' Optionally filters by specific drug concepts or cohort membership.
#'
#' @param cdm A cdm_reference object created by CDMConnector, providing access to
#'   OMOP CDM tables including drug_exposure, person, observation_period, and concept.
#' @param name Character string specifying the name for the output table in the
#'   CDM write schema. Default is "adherenceFromOMOP".
#' @param indexDateForPatientCalculation Default is "drug_exposure_start_date".
#' @param overwrite Logical. If TRUE, overwrites existing table with the same name.
#'   If FALSE and table exists, returns NULL with a warning. Default is FALSE.
#' @param subjects Optional numeric vector of person_id values to include. If NULL,
#'   all subjects are included. Useful for testing or analyzing specific patients.
#' @param drugConceptIdList Optional numeric vector of drug_concept_id values to filter
#'   drug exposure records. If NULL, all drug exposures are included.
#' @param cohortSchema Optional character string specifying the database schema containing
#'   a cohort table. When provided with cohortId, only drug exposures within cohort
#'   periods are included.
#' @param cohortId Optional numeric vector of cohort_definition_id values to filter patients.
#'   Used together with cohortSchema to restrict to patients in specific cohorts.
#' @param cohortTableName Character string specifying the cohort table name in cohortSchema.
#'   Default is "cohort".
#' @param ... Additional arguments (currently unused, reserved for future extensions).
#'
#' @return A database table reference containing drug exposure records with columns:
#'   person_id, drug_concept_id, drug_exposure_start_date, days_supply, date_of_birth,
#'   sex, date_0f_death, observation_period_start_date, observation_period_end_date.
#'   When cohortSchema is used, also includes cohort_start_date and cohort_end_date.
#'   Returns NULL if validation fails or no records match the criteria.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#'
#' # Include all drugs
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   name = "chronic_drug_exposure_table",
#'   overwrite = TRUE
#' )
#'
#' # Filter by specific drug concepts
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   drugConceptIdList = c(1234567, 2345678),
#'   name = "filtered_exposure",
#'   overwrite = TRUE
#' )
#' }
generateChronicDrugExposure <- function(cdm,
                                        name = "adherenceFromOMOP",
                                        indexDateForPatientCalculation = "drug_exposure_start_date",
                                        overwrite = FALSE,
                                        subjects = NULL,
                                        drugConceptIdList = NULL,
                                        cohortSchema = NULL,
                                        cohortId = NULL,
                                        cohortTableName = "cohort",
                                        ...) {
  if (name %in% (!name %in% CDMConnector::listTables(CDMConnector::cdmCon(cdm), schema = CDMConnector::cdmWriteSchema(cdm))) &
    !overwrite) {
    cli::cli_alert_warning("Already has table named { name }. Set overwrite = TRUE to write over specified table.")
    return(NULL)
  }


  if (is.null(drugConceptIdList)) {
    cli::cli_alert_info("Drug concept ids not provided, all drugs included.")

    data <- cdm$drug_exposure
  } else {
    cli::cli_alert_info("Using the drug concept id list")

    data <- dplyr::filter(cdm$drug_exposure, drug_concept_id %in% drugConceptIdList)
  }

  cli::cli_alert_info("Using Patient Profiles package to add patient data.")

  data <- data %>%
    dplyr::left_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::inner_join(cdm$observation_period, by = c("person_id")) %>%
    PatientProfiles::addDateOfBirth() %>%
    PatientProfiles::addSex() %>%
    #PatientProfiles::addDeathDays(indexDate = indexDateForPatientCalculation) %>%
    PatientProfiles::addDeathDate(indexDate = indexDateForPatientCalculation) %>%
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
      #days_to_death,
      date_of_death,
      observation_period_end_date,
      observation_period_start_date
    ) %>%
    dplyr::compute(
      name = name,
      temporary = FALSE,
      schema = CDMConnector::cdmWriteSchema(cdm),
      overwrite = TRUE
    )

  if (!is.null(cohortSchema)) {
    connection <- CDMConnector::cdmCon(cdm)
    cohort <- dplyr::tbl(connection, CDMConnector::inSchema(cohortSchema, cohortTableName)) %>%
      dplyr::filter(cohort_definition_id %in% cohortId)

    if (nrow(dplyr::collect(cohort, n = 1)) < 1) {
      cli::cli_alert_warning("Check again, cohort seems empty. Numer of rows is less than 1.")
      return(NULL)
    }

    data <- data %>%
      dplyr::inner_join(cohort, dplyr::join_by(x$person_id == y$subject_id)) %>%
      dplyr::filter(
        drug_exposure_start_date >= cohort_start_date &
          drug_exposure_start_date <= cohort_end_date
      ) # %>%
    # dplyr::select(
    #   person_id,
    #   drug_concept_id,
    #   #concept_name,
    #   drug_exposure_start_date,
    #   days_supply,
    #   #visit_occurrence_id,
    #   #route_concept_id,
    #   #route_source_value,
    #   date_of_birth,
    #   sex,
    #   days_to_death,
    #   cohort_start_date,
    #   cohort_end_date,
    #   observation_period_end_date,
    #   observation_period_start_date
    # )
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

  chronicDrugExposure <- data %>% dplyr::compute(
    name = name,
    temporary = FALSE,
    schema = CDMConnector::cdmWriteSchema(cdm),
    overwrite = TRUE
  )

  return(chronicDrugExposure)
}
