#' Create a mock OMOP CDM database for testing adherence calculations
#'
#' Generates a synthetic OMOP CDM database in DuckDB with all tables required for
#' adherence calculations: person, observation_period, drug_exposure, measurement,
#' death, and vocabulary tables. The drug_exposure table is populated with computed
#' days_supply values. Useful for unit testing and demonstrating package functionality
#' without requiring access to real patient data.
#'
#' @param nPerson Integer specifying the number of synthetic patients to generate.
#'   Passed to omock::mockPerson. Default is 5.
#' @param recordPerson Integer specifying the number of drug exposure and measurement
#'   records to generate per person. Passed to omock::mockDrugExposure and
#'   omock::mockMeasurement. Default is 3.
#' @param dbms Database management system connection object. Currently only DuckDB
#'   is supported. Default is duckdb::duckdb().
#'
#' @returns A cdm_reference object connected to an in-memory DuckDB database containing
#'   synthetic OMOP CDM tables. The write schema is set to "main" with prefix "adherencemock_".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
#' cdm <- mockAdherenceFromOMOP(nPerson = 10, recordPerson = 5)
#' }
mockAdherenceFromOMOP <- function(nPerson = 5, recordPerson = 3, dbms = duckdb::duckdb()) {
  cdm <- omopgenerics::emptyCdmReference(cdmName = "mock")

  cdm <- cdm %>%
    omock::mockPerson(nPerson = nPerson) %>%
    omock::mockVocabularyTables() %>%
    omock::mockObservationPeriod() %>%
    omock::mockDrugExposure(recordPerson = recordPerson) %>%
    omock::mockMeasurement(recordPerson = recordPerson)

  # days supply otherwise missing
  cdm$drug_exposure <- cdm$drug_exposure %>%
    dplyr::mutate(days_supply = as.integer(.data$drug_exposure_end_date - .data$drug_exposure_start_date))

  cdm <- cdm %>%
    omock::mockDeath()

  # con <- duckdb::dbConnect(dbms, ":memory:")
  con <- DBI::dbConnect(dbms, ":memory:")

  CDMConnector::copyCdmTo(con, cdm, schema = "main", overwrite = FALSE)

  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main", writePrefix = "adherencemock_")

  return(cdm)
}
