testthat::test_that("testing summarisePatientCounts", {
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  counts <- summarisePatientCounts(cdm = cdm, adherenceData = CMA_values, drugExposure = chronicDrugExposure)

  testthat::expect_true(!is.null(counts))
})


testthat::test_that("addBMI testing", {
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  withBMI <- addBMI(CMA_values, cdm)

  testthat::expect_true(!is.null(withBMI))
})

testthat::test_that("addCohorts testing", {
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  # Create mock cohort data directly instead of reading from files
  observ_dates <- cdm$observation_period %>%
    dplyr::filter(person_id == 1) %>%
    dplyr::select(observation_period_start_date, observation_period_end_date) %>%
    dplyr::collect()

  cohortTable <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 1,
    cohort_start_date = observ_dates$observation_period_start_date,
    cohort_end_date = observ_dates$observation_period_end_date
  )

  cdm <- omopgenerics::insertTable(cdm, name = "test_cohort", table = cohortTable)

  withCohort <- addCohorts(CMA_values, cdm$test_cohort)
  #withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))
})

testthat::test_that("addCohorts testing with sliding window", {
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  cma_types <- c("CMA1", "CMA2", "CMA3")
  CMA_values <- calculateAdherenceSlidingWindowBatched(cdm = cdm, drugExposure = chronicDrugExposure, cma = cma_types)

  # Create mock cohort data directly instead of reading from files
  observ_dates <- cdm$observation_period %>%
    dplyr::filter(person_id == 1) %>%
    dplyr::select(observation_period_start_date, observation_period_end_date) %>%
    dplyr::collect()

  cohortTable <- dplyr::tibble(
    cohort_definition_id = c(1,2),
    subject_id = c(1,2),
    cohort_start_date = c(observ_dates$observation_period_start_date,observ_dates$observation_period_start_date),
    cohort_end_date = c(observ_dates$observation_period_end_date, observ_dates$observation_period_end_date)
  )
  cdm <- omopgenerics::insertTable(cdm, name = "test_cohort", table = cohortTable)

  withCohort <- addCohorts(CMA_values, cdm$test_cohort)
  #withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))
})

testthat::test_that("addbmi testing with sliding window", {
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  cma_types <- c("CMA1", "CMA2", "CMA3")
  CMA_values <- calculateAdherenceSlidingWindowBatched(cdm = cdm, drugExposure = chronicDrugExposure, cma = cma_types)

  withBMI <- addBMI(CMA_values, cdm)
  #cdm <- omopgenerics::insertTable(cdm, name = "test_cohort", table = cohortTable)

  #withCohort <- addCohorts(CMA_values, cdm$test_cohort)

  testthat::expect_true(!is.null(withBMI))
})

testthat::test_that("testing summarisePatientCounts with cohort", {
  cdm <- mockDrugExposure()

  observ_dates <- cdm$observation_period %>%
    dplyr::filter(person_id == 1) %>%
    dplyr::select(observation_period_start_date, observation_period_end_date)

  cohort_table <- dplyr::tibble(
    cohort_definition_id = 1,
    cohort_start_date = dplyr::pull(observ_dates, observation_period_start_date),
    cohort_end_date = dplyr::pull(observ_dates, observation_period_end_date)
  )
  DBI::dbWriteTable(CDMConnector::cdmCon(cdm), "cohort", cohort_table)

  cohort_ref <- dplyr::tbl(CDMConnector::cdmCon(cdm), "cohort")

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  #omopgenerics::newCdmTable(table = cohort_table, src = cdm, name = "cohort1")
  #omopgenerics::newCohortTable(cohort_table)

  counts <- summarisePatientCounts(cdm = cdm, adherenceData = CMA_values, drugExposure = chronicDrugExposure)

  testthat::expect_true(!is.null(counts))
})

testthat::test_that("addBMI keeps one row per window when several BMI measurements fall in the year before it", {
  cdm <- mockDrugExposure()
  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)
  CMA_values <- suppressWarnings(calculateAdherenceSlidingWindowBatched(
    cdm = cdm, drugExposure = chronicDrugExposure, cma = "CMA7", name = "bmi_sw"
  ))

  # quarterly BMI measurements for everyone, covering the year before the first window
  persons <- dplyr::pull(dplyr::distinct(CMA_values, person_id))
  range <- dplyr::collect(dplyr::summarise(CMA_values, lo = min(window.start, na.rm = TRUE), hi = max(window.end, na.rm = TRUE)))
  measurement <- tidyr::expand_grid(
    person_id = persons,
    measurement_date = seq(range$lo - 365, range$hi, by = "quarter")
  ) %>%
    dplyr::mutate(
      measurement_concept_id = 4245997L,
      unit_source_value = "kg/m2",
      # value encodes the date, so the chosen measurement can be checked
      value_source_value = as.character(as.numeric(measurement_date))
    )
  # addBMI only needs cdm$measurement
  bmiCdm <- list(measurement = omopgenerics::insertTable(cdm, name = "bmi_measurement", table = measurement)$bmi_measurement)

  withBMI <- addBMI(CMA_values, bmiCdm, name = "bmi_sw_result", overwrite = TRUE) %>% dplyr::collect()
  nWindows <- CMA_values %>% dplyr::count() %>% dplyr::pull()

  testthat::expect_equal(nrow(withBMI), nWindows)
  testthat::expect_equal(nrow(dplyr::distinct(withBMI, name, person_id, group, window.start)), nWindows)

  # the latest measurement on or before window.start (within a year) is used
  expected <- withBMI %>%
    dplyr::select(name, person_id, group, window.start) %>%
    dplyr::inner_join(measurement, by = "person_id", relationship = "many-to-many") %>%
    dplyr::filter(measurement_date <= window.start, measurement_date >= window.start - 365) %>%
    dplyr::group_by(name, person_id, group, window.start) %>%
    dplyr::summarise(expected_date = max(measurement_date), .groups = "drop")
  check <- dplyr::inner_join(withBMI, expected, by = c("name", "person_id", "group", "window.start"))
  testthat::expect_equal(nrow(check), nWindows)
  testthat::expect_equal(check$measurement_date, check$expected_date)
})

testthat::test_that("addCohorts counts cohort time per window even when window.id repeats", {
  cdm <- mockDrugExposure()
  # two windows of one person share window.id (as after cleanNARows), cohort overlaps only the first
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
  # written like the batched functions write results (keeps mixed-case column names)
  con <- CDMConnector::cdmCon(cdm)
  DBI::dbWriteTable(con, "test_adherence", adherence, overwrite = TRUE)
  cdm <- omopgenerics::insertTable(cdm, name = "test_cohort", table = cohortTable)

  result <- addCohorts(dplyr::tbl(con, "test_adherence"), cdm$test_cohort) %>%
    dplyr::collect() %>%
    dplyr::arrange(window.start)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$cohort_1, c(1, 0))
  testthat::expect_equal(result$time_in_1, c(10, 0))
  # other columns are kept, e.g. when used after addBMI
  testthat::expect_equal(result$bmi_category, c("Obese", "Overweight"))
})
