testthat::test_that("batched calculation without logFile writes no logs", {
  mock_cdm <- mockDrugExposure(nPerson = 5)
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm, conceptSet = NULL, name = "test_data", overwrite = T
  )
  dir <- withr::local_tempdir()
  result <- suppressWarnings(calculateAdherenceBatched(
    cdm = mock_cdm, drugExposure = data, cma = "CMA5", name = "no_log"
  ))
  testthat::expect_false(is.null(result))
  testthat::expect_length(list.files(dir), 0)
  testthat::expect_false(loggingActive())
})

testthat::test_that("batched calculation writes run log with batches and summary", {
  mock_cdm <- mockDrugExposure(nPerson = 6)
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm, conceptSet = NULL, name = "test_data", overwrite = T
  )
  dir <- withr::local_tempdir()
  logFile <- file.path(dir, "run.log")

  result <- suppressWarnings(calculateAdherenceBatched(
    cdm = mock_cdm, drugExposure = data, cma = "CMA5", name = "with_log",
    batchSize = 2, logFile = logFile
  ))

  testthat::expect_true(file.exists(logFile))
  testthat::expect_true(file.exists(file.path(dir, "run_errors.log")))
  log <- readLines(logFile)
  testthat::expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\[", log)))
  testthat::expect_equal(sum(grepl("\\[batch \\d/3\\] Started", log)), 3)
  testthat::expect_equal(sum(grepl("\\[batch \\d/3\\] Finished", log)), 3)
  testthat::expect_true(any(grepl("Result summary", log)))
  testthat::expect_true(any(grepl("Distinct persons: 6", log)))
  # logger is switched off after the run
  testthat::expect_false(loggingActive())
})

testthat::test_that("problems are written to the error log", {
  mock_cdm <- mockDrugExposure(nPerson = 10)
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm, conceptSet = NULL, name = "test_data", overwrite = T
  ) %>% dplyr::collect()

  # exposures before the observation period make this person fail in AdhereR
  shiftedPerson <- data$person_id[1]
  firstExposure <- min(data$drug_exposure_start_date[data$person_id == shiftedPerson])
  data$observation_period_start_date[data$person_id == shiftedPerson] <- firstExposure + 10

  dir <- withr::local_tempdir()
  logFile <- file.path(dir, "sw.log")
  errorLogFile <- file.path(dir, "errors.txt")

  result <- suppressWarnings(calculateAdherenceSlidingWindowBatched(
    cdm = mock_cdm, drugExposure = data, cma = "CMA6", name = "sw_log",
    batchSize = 5, delayObservationWindowStart = TRUE,
    logFile = logFile, errorLogFile = errorLogFile
  ))

  errors <- readLines(errorLogFile)
  testthat::expect_true(any(grepl("\\[WARNING", errors) & grepl(paste0("participant\\(s\\) ", shiftedPerson), errors)))
  testthat::expect_true(all(grepl("\\[batch \\d/2\\]", errors)))
  # problems are not in the run log, but counted there
  log <- readLines(logFile)
  testthat::expect_false(any(grepl("\\[WARNING", log)))
  testthat::expect_true(any(grepl("[1-9]\\d* problem\\(s\\) logged", log)))
})

testthat::test_that("error log can be used without run log", {
  mock_cdm <- mockDrugExposure(nPerson = 4)
  data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm, conceptSet = NULL, name = "test_data", overwrite = T
  )
  dir <- withr::local_tempdir()
  errorLogFile <- file.path(dir, "errors.log")
  suppressWarnings(calculateAdherenceBatched(
    cdm = mock_cdm, drugExposure = data, cma = "CMA5", name = "err_only",
    errorLogFile = errorLogFile
  ))
  testthat::expect_equal(list.files(dir), "errors.log")
})

testthat::test_that("defaultErrorLogFile derives the error log name", {
  testthat::expect_equal(defaultErrorLogFile("a/run.log"), "a/run_errors.log")
  testthat::expect_equal(defaultErrorLogFile("a/run"), "a/run_errors")
  testthat::expect_equal(defaultErrorLogFile("a.b/run"), "a.b/run_errors")
})

testthat::test_that("summariseAdherence counts persons and groups", {
  adherence <- data.frame(
    name = c("CMA5", "CMA5", "CMA5", "CMA7"),
    person_id = c(1, 2, 1, 1),
    group = c("a", "a", "b", "a"),
    CMA = c(0.5, 1, 0.8, 0.9)
  )
  summary <- summariseAdherence(adherence)
  testthat::expect_equal(summary$overview$rows, 4)
  testthat::expect_equal(summary$overview$persons, 2)
  testthat::expect_equal(summary$overview$cmas, 2)
  testthat::expect_equal(summary$overview$groups, 2)
  cma5a <- summary$byGroup[summary$byGroup$name == "CMA5" & summary$byGroup$group == "a", ]
  testthat::expect_equal(cma5a$persons, 2)
  testthat::expect_equal(cma5a$mean_cma, 0.75)
  testthat::expect_equal(summary$byCma$persons[summary$byCma$name == "CMA5"], 2)

  testthat::expect_error(summariseAdherence(data.frame(person_id = 1)))
})
