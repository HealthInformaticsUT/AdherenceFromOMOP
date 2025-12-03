#' Get drug exposure entries for adherence calculations
#'
#' @param indexDateForPatientCalculation Value for indexDate in addDeathDays function from PatientProfiles.
#' @param name Name of the new cohort table, it must be a length 1 character vector.
#' @param overwrite If table with provided name exists this value chooses the action.
#' @param subjects Possibility to choose a specific set of subjects.
#' @param ... Arguments to be passed to CodelistGenerator \link[CodelistGenerator]{getDrugIngredientCodes} function.
#' @param cohortSchema Possible include only certain people that exist in a specific defined cohort between a time period. Parameter cohort schema must be a string value of the database schema with the defined cohort.
#' @param cohortId List of numeric values of the defined cohorts that haveS included person ids as well as start and end date of the cohort.
#' @param drugConceptIdList list with drug concept ids that determine if drug exposure records are included or not
#' @param cohortTableName usually named "cohort"
#' @param cdm A cdm reference via CDMConnector.
#'
#' @return reference to table
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#'
#' chronicDrugExposure <- generateChronicDrugExposure(
#'   cdm = cdm,
#'   drugConceptIdList = concepts_ids,
#'   name = "chronic_drug_exposure_table",
#'   overwrite = T
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

    data <- dplyr::filter(cdm$drug_exposure, drug_concept_id %in% !!drugConceptIdList)
  }

  cli::cli_alert_info("Using Patient Profiles package to add patient data.")

  data <- data %>%
    dplyr::left_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::filter(concept_class_id %like% "%Drug%") %>%
    dplyr::inner_join(cdm$observation_period, by = c("person_id")) %>%
    PatientProfiles::addDateOfBirth() %>%
    PatientProfiles::addSex() %>%
    PatientProfiles::addDeathDays(indexDate = indexDateForPatientCalculation) %>%
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
      days_to_death,
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
