# Extracted from test-addFeatures.R:186

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "AdherenceFromOMOP", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cdm <- mockDrugExposure()
adherence <- dplyr::tibble(
    name = "CMA7",
    person_id = 1L,
    group = "all",
    window.ID = c(1L, 1L),
    window.start = as.Date(c("2010-01-01", "2011-01-01")),
    window.end = as.Date(c("2010-12-31", "2011-12-31")),
    CMA = c(0.5, 0.9),
    bmi_category = c("Obese", "Overweight")
  )
cohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2010-03-01"),
    cohort_end_date = as.Date("2010-03-10")
  )
cdm <- omopgenerics::insertTable(cdm, name = "test_adherence", table = adherence)
