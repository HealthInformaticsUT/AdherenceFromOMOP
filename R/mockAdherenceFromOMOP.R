#' Mock database for testing package
#'
#' Creates a mock CDM database with drug exposure data for testing and examples.
#'
#' @param nPerson (`numeric(1)`) Number of persons to generate in the mock database.
#'   Default: 5.
#' @param recordPerson (`numeric(1)`) Number of drug exposure records per person.
#'   Default: 3.
#' @param dbms Database management system driver. Default: `duckdb::duckdb()`.
#'
#' @returns A `cdm_reference` object with mock tables including person, drug_exposure,
#'   observation_period, measurement, and death tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#' }
mockDrugExposure <- function(nPerson = 5, recordPerson = 3, dbms = duckdb::duckdb()) {
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
