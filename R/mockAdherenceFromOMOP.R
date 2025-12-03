#' Mock database for testing package.
#'
#' @param nPerson numeric value for omock::mockPerson \link[omock]{mockPerson} function.
#' @param recordPerson numeric value for omock::mockDrugExposure \link[omock]{mockDrugExposure} function.
#' @param dbms currently accepts only duckdb
#'
#' @returns CDM reference with necessary tables
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockAdherenceFromOMOP()
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
