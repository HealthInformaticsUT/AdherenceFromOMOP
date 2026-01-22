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
