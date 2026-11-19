# test for generateChronicDrugExposure fucntion
testthat::test_that("correct general use case", {
  # db connection
  cdm <- mockDrugExposure()
  # function under test
  reference <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    conceptSet = NULL,
    name = "test_table",
    overwrite = T
  )

  testthat::expect_true(!is.null(reference))
  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("faulty conceptSet", {
  # db connection
  cdm <- mockDrugExposure()

  # function under test
  testthat::expect_error(data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    conceptSet = c("random_drug"),
    name = "test_table",
    overwrite = T
  ))
  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("faulty subjects", {
  # db connection
  cdm <- mockDrugExposure()

  # function under test
  testthat::expect_null(AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    conceptSet = c(1361364),
    name = "test_table",
    overwrite = T,
    subjects = c(-1)
  ))
  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("correct general use case with cohort", {
  cdm <- mockDrugExposure()

  cohort_table <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = as.Date("1980-01-01"),
    cohort_end_date = as.Date("2020-01-10")
  )
  DBI::dbWriteTable(CDMConnector::cdmCon(cdm), "cohort", cohort_table)

  cohort_ref <- dplyr::tbl(CDMConnector::cdmCon(cdm), "cohort")

  # function under test
  AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    name = "test_table",
    overwrite = T,
    cohort = cohort_ref,
    cohortId = 1
  )
  testthat::expect_true("adherencemock_test_table" %in% DBI::dbListTables(CDMConnector::cdmCon(cdm)))
  testthat::expect_true("cohort" %in% DBI::dbListTables(CDMConnector::cdmCon(cdm)))

  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("date_of_death is the same on all records of a person, also records after death", {
  cdm <- mockDrugExposure(nPerson = 5)

  # move one person's death to the middle of their drug exposures
  exposures <- cdm$drug_exposure %>% dplyr::collect()
  person <- exposures$person_id[1]
  personDates <- sort(exposures$drug_exposure_start_date[exposures$person_id == person])
  deathDate <- personDates[ceiling(length(personDates) / 2)]
  death <- cdm$death %>%
    dplyr::collect() %>%
    dplyr::mutate(death_date = dplyr::if_else(person_id == person, deathDate, death_date))
  cdm <- omopgenerics::insertTable(cdm, name = "death", table = death)

  data <- generateChronicDrugExposure(cdm = cdm, conceptSet = NULL, name = "test_data", overwrite = T) %>%
    dplyr::collect()

  personData <- data[data$person_id == person, ]
  testthat::expect_true(any(personData$drug_exposure_start_date > deathDate))
  testthat::expect_true(all(personData$date_of_death == deathDate))
  # one date of death per person
  testthat::expect_equal(nrow(dplyr::distinct(data, person_id, date_of_death)), dplyr::n_distinct(data$person_id))

  # so adherence results are not duplicated
  result <- suppressWarnings(calculateAdherenceSlidingWindow(drugExposure = data, cma = "CMA7"))
  testthat::expect_equal(nrow(result), nrow(dplyr::distinct(result, name, person_id, group, window.start)))
})
