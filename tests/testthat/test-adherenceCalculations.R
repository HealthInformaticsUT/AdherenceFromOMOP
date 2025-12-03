# test for adherenceCalculations function
testthat::test_that("correct general use case", {
  # db connection
  cdm <- mockAdherenceFromOMOP()
  # function under test
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    ingredients = NULL,
    name = "test_table",
    overwrite = T
  )
  simpleCMA <- adherenceCalculationsLoader(cdm = cdm, reference = data, cma = "CMA5")
  testthat::expect_true(!is.null(simpleCMA))
  # testthat::expect_true("test_table" %in% CDMConnector::listTables(con = CDMConnector::cdmCon(cdm)))
  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("adherenceCalculations returns correct structure", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- AdherenceFromOMOP::adherenceCalculationsLoader(cdm = mock_cdm, reference = data, cma = "CMA1")

  testthat::expect_type(result, "list")
  testthat::expect_true("CMA1" %in% unique(dplyr::pull(result, name)))
}) #> Test passed 🎉

testthat::test_that("function works with multiple CMAs", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  cma_types <- c("CMA1", "CMA2", "CMA3")
  result <- adherenceCalculationsLoader(cdm = mock_cdm, reference = data, cma = cma_types)

  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("CMA1", "CMA2", "CMA3") %in% unique(dplyr::pull(result, name))))
}) #> Test passed 🎉

testthat::test_that("function handles invalid CMA types gracefully", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- adherenceCalculationsLoader(cdm = mock_cdm, reference = data, cma = c("CMA1", "INVALID_CMA"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles missing columns in input data", {
  mock_cdm <- mockAdherenceFromOMOP()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  data <- dplyr::collect(ref)
  data <- dplyr::select(data, -days_supply)

  result <- adherenceCalculationsLoader(cdm = mock_cdm, reference  = data, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles empty data frames", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T,
    subjects = 8
  )

  result <- adherenceCalculationsLoader(cdm = mock_cdm, reference = data, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉






# tests for adherenceSlidingWindow
testthat::test_that("adherenceSlidingWindow returns correct structure", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- AdherenceFromOMOP::adherenceSlidingWindowLoader(cdm = mock_cdm, reference = data, cma = "CMA1")

  testthat::expect_type(result, "list")
  testthat::expect_true("CMA1" %in% unique(dplyr::pull(result, name)))
}) #> Test passed 🎉

testthat::test_that("function works with multiple CMAs", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  cma_types <- c("CMA1", "CMA2", "CMA3")
  result <- adherenceSlidingWindowLoader(cdm = mock_cdm, reference = data, cma = cma_types)

  testthat::expect_true(all(cma_types %in% unique(dplyr::pull(result, name))))
}) #> Test passed 🎉

testthat::test_that("function handles invalid CMA types gracefully", {
  mock_cdm <- mockAdherenceFromOMOP()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- adherenceSlidingWindowLoader(cdm = mock_cdm, reference = data, cma = c("CMA1", "INVALID_CMA"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles missing columns in input data", {
  mock_cdm <- mockAdherenceFromOMOP()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T
  )
  data <- dplyr::collect(ref)
  data <- dplyr::select(data, -days_supply)

  result <- adherenceSlidingWindow(cdm = mock_cdm, data = data, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles empty data frames", {
  mock_cdm <- mockAdherenceFromOMOP()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    ingredients = NULL,
    name = "test_data",
    overwrite = T,
    subjects = 8
  )

  result <- adherenceSlidingWindow(cdm = mock_cdm, reference = ref, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉


test_that("function handles NULL reference parameter", {
  mock_cdm <- mockAdherenceFromOMOP()

  result <- adherenceSlidingWindow(cdm = mock_cdm, reference = NULL, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉

test_that("function handles NULL reference parameter", {
  mock_cdm <- mockAdherenceFromOMOP()

  result <- adherenceSlidingWindowLoader(cdm = mock_cdm, reference = NULL, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("correct general use case", {
  cdm <- mockAdherenceFromOMOP()

  observ_dates <- cdm$observation_period %>%
    dplyr::filter(person_id == 1) %>%
    dplyr::select(observation_period_start_date, observation_period_end_date)

  cohort <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = dplyr::pull(observ_dates, observation_period_start_date),
    cohort_end_date = dplyr::pull(observ_dates, observation_period_end_date)
  )
  DBI::dbWriteTable(CDMConnector::cdmCon(cdm), "cohort", cohort)

  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    name = "test_table",
    overwrite = T,
    cohortSchema = "main",
    cohortId = 1,
  )

  result <- adherenceSlidingWindowLoader(cdm = cdm, reference = data, cma = c("CMA1"))

  testthat::expect_true(!is.null(result))

  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉
