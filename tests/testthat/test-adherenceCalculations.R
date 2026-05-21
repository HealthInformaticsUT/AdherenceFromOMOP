# test for calculateAdherence function
testthat::test_that("correct general use case", {
  # db connection
  cdm <- mockDrugExposure()
  # function under test
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    conceptSet = NULL,
    name = "test_table",
    overwrite = T
  )
  simpleCMA <- calculateAdherenceBatched(cdm = cdm, drugExposure = data, cma = "CMA5")
  testthat::expect_true(!is.null(simpleCMA))
  # testthat::expect_true("test_table" %in% CDMConnector::listTables(con = CDMConnector::cdmCon(cdm)))
  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

testthat::test_that("calculateAdherence returns correct structure", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- AdherenceFromOMOP::calculateAdherenceBatched(cdm = mock_cdm, drugExposure = data, cma = "CMA1")

  testthat::expect_type(result, "list")
  testthat::expect_true("CMA1" %in% unique(dplyr::pull(result, name)))
}) #> Test passed 🎉

testthat::test_that("function works with multiple CMAs", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  cma_types <- c("CMA1", "CMA2", "CMA3")
  result <- calculateAdherenceBatched(cdm = mock_cdm, drugExposure = data, cma = cma_types)

  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("CMA1", "CMA2", "CMA3") %in% unique(dplyr::pull(result, name))))
}) #> Test passed 🎉

testthat::test_that("function handles invalid CMA types gracefully", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- calculateAdherenceBatched(cdm = mock_cdm, drugExposure = data, cma = c("CMA1", "INVALID_CMA"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles missing columns in input data", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  data <- dplyr::collect(ref)
  data <- dplyr::select(data, -days_supply)

  result <- calculateAdherenceBatched(cdm = mock_cdm, drugExposure = data, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles empty data frames", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T,
    subjects = 8
  )

  result <- calculateAdherenceBatched(cdm = mock_cdm, drugExposure = data, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉


# Direct tests for calculateAdherence function (not Batched)
testthat::test_that("calculateAdherence works with in-memory data", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = TRUE
  )
  # Collect data into memory
  in_memory_data <- dplyr::collect(ref)

  # Call calculateAdherence directly with in-memory data
  result <- calculateAdherence(drugExposure = in_memory_data, cma = "CMA5")

  testthat::expect_false(is.null(result))
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("CMA5" %in% unique(result$name))
  testthat::expect_true("person_id" %in% colnames(result))
  testthat::expect_true("CMA" %in% colnames(result))
})

testthat::test_that("calculateAdherence works with multiple CMA types", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = TRUE
  )
  in_memory_data <- dplyr::collect(ref)

  cma_types <- c("CMA1", "CMA2", "CMA5")
  result <- calculateAdherence(drugExposure = in_memory_data, cma = cma_types)

  testthat::expect_false(is.null(result))
  testthat::expect_true(all(cma_types %in% unique(result$name)))
})

testthat::test_that("calculateAdherence handles invalid CMA types", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = TRUE
  )
  in_memory_data <- dplyr::collect(ref)

  result <- calculateAdherence(drugExposure = in_memory_data, cma = c("CMA1", "INVALID_CMA"))

  testthat::expect_null(result)
})

testthat::test_that("calculateAdherence handles NULL data", {
  result <- calculateAdherence(drugExposure = NULL, cma = "CMA1")

  testthat::expect_null(result)
})

testthat::test_that("calculateAdherence handles data with missing required columns", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = TRUE
  )
  in_memory_data <- dplyr::collect(ref)
  # Remove a required column
  in_memory_data <- dplyr::select(in_memory_data, -days_supply)

  result <- calculateAdherence(drugExposure = in_memory_data, cma = "CMA1")

  testthat::expect_null(result)
})

testthat::test_that("calculateAdherence works with all CMA types (CMA1-CMA9)", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = TRUE
  )
  in_memory_data <- dplyr::collect(ref)

  all_cmas <- c("CMA1", "CMA2", "CMA3", "CMA4", "CMA5", "CMA6", "CMA7", "CMA8", "CMA9")
  result <- calculateAdherence(drugExposure = in_memory_data, cma = all_cmas)

  testthat::expect_false(is.null(result))
  testthat::expect_true(all(all_cmas %in% unique(result$name)))
})




# tests for calculateAdherenceSlidingWindow
testthat::test_that("calculateAdherenceSlidingWindow returns correct structure", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- AdherenceFromOMOP::calculateAdherenceSlidingWindowBatched(cdm = mock_cdm, drugExposure = data, cma = "CMA1")

  testthat::expect_type(result, "list")
  testthat::expect_true("CMA1" %in% unique(dplyr::pull(result, name)))
}) #> Test passed 🎉

testthat::test_that("function works with multiple CMAs", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  cma_types <- c("CMA1", "CMA2", "CMA3")
  result <- calculateAdherenceSlidingWindowBatched(cdm = mock_cdm, drugExposure = data, cma = cma_types)

  testthat::expect_true(all(cma_types %in% unique(dplyr::pull(result, name))))
}) #> Test passed 🎉

testthat::test_that("function handles invalid CMA types gracefully", {
  mock_cdm <- mockDrugExposure()
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  result <- calculateAdherenceSlidingWindowBatched(cdm = mock_cdm, drugExposure = data, cma = c("CMA1", "INVALID_CMA"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles missing columns in input data", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T
  )
  data <- dplyr::collect(ref)
  data <- dplyr::select(data, -days_supply)

  result <- calculateAdherenceSlidingWindow(cdm = mock_cdm, drugExposure = data, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("function handles empty data frames", {
  mock_cdm <- mockDrugExposure()
  ref <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm,
    conceptSet = NULL,
    name = "test_data",
    overwrite = T,
    subjects = 8
  )

  result <- calculateAdherenceSlidingWindow(cdm = mock_cdm, drugExposure = ref, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉


test_that("function handles NULL reference parameter", {
  mock_cdm <- mockDrugExposure()

  result <- calculateAdherenceSlidingWindow(cdm = mock_cdm, drugExposure = NULL, cma = "CMA1")

  testthat::expect_null(result)
}) #> Test passed 🎉

test_that("function handles NULL reference parameter", {
  mock_cdm <- mockDrugExposure()

  result <- calculateAdherenceSlidingWindowBatched(cdm = mock_cdm, drugExposure = NULL, cma = c("CMA1"))

  testthat::expect_null(result)
}) #> Test passed 🎉

testthat::test_that("correct general use case with cohort", {
  cdm <- mockDrugExposure()

  observ_dates <- cdm$observation_period %>%
    dplyr::filter(person_id == 1) %>%
    dplyr::select(observation_period_start_date, observation_period_end_date)

  cohort_table <- dplyr::tibble(
    cohort_definition_id = 1, subject_id = 1,
    cohort_start_date = dplyr::pull(observ_dates, observation_period_start_date),
    cohort_end_date = dplyr::pull(observ_dates, observation_period_end_date)
  )
  DBI::dbWriteTable(CDMConnector::cdmCon(cdm), "cohort", cohort_table)

  cohort_ref <- dplyr::tbl(CDMConnector::cdmCon(cdm), "cohort")

  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = cdm,
    name = "test_table",
    overwrite = T,
    cohort = cohort_ref,
    cohortId = 1
  )

  result <- calculateAdherenceSlidingWindowBatched(cdm = cdm, drugExposure = data, cma = c("CMA1"))

  testthat::expect_true(!is.null(result))

  # DBI::dbDisconnect(connection)
}) #> Test passed 🎉

# medication Grouping
#####

testthat::test_that("returns NULL and emits message when ingredientGroups is NULL", {
  mock_cdm <- mockDrugExposure()

  expect_message(
    result <- medicationGrouping(mock_cdm, ingredientGroups = NULL),
    "Medication groups not provided"
  )
  expect_null(result)
})

testthat::test_that("uses concept_name nameStyle when ingredients are character names", {
  mock_cdm <- CodelistGenerator::mockVocabRef()

  groups <- medicationGrouping(mock_cdm, ingredientGroups=list(group1 = c("Adalimumab")))

  expect_equal(length(groups), 1)
})

test_that("uses concept_code nameStyle when ingredients are numeric codes", {
  mock_cdm <- CodelistGenerator::mockVocabRef()

  groups <- medicationGrouping(mock_cdm, ingredientGroups=list(group1 = c(10)))

  expect_equal(length(groups), 1)
})

test_that("groups are correctly merged from individual codelist entries", {

  mock_cdm <- CodelistGenerator::mockVocabRef()

  groups <- medicationGrouping(mock_cdm, ingredientGroups=list(group1 = c("Adalimumab", "Other ingredient")))

  expect_equal(length(groups), 1)
})

