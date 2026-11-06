# Extracted from test-logging.R:62

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "AdherenceFromOMOP", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
mock_cdm <- mockDrugExposure(nPerson = 10)
data <- AdherenceFromOMOP::generateChronicDrugExposure(
    cdm = mock_cdm, conceptSet = NULL, name = "test_data", overwrite = T
  ) %>% dplyr::collect()
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
testthat::expect_true(any(grepl("\\[ERROR", errors) & grepl(shiftedPerson, errors)))
