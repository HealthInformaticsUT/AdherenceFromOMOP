testthat::test_that("testing summarisePatientCounts", {

  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  counts <- summarisePatientCounts(cdm = cdm, adherenceData = CMA_values, drugExposure = chronicDrugExposure)

  testthat::expect_true(!is.null(counts))
})


testthat::test_that("addBMI testing",{
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  withBMI <- addBMI(CMA_values, cdm)

  testthat::expect_true(!is.null(withBMI))

})

testthat::test_that("addCohorts testing",{
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

  withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))

})

testthat::test_that("addCohorts testing with sliding window",{
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
    cohort_definition_id = 1,
    subject_id = 1,
    cohort_start_date = observ_dates$observation_period_start_date,
    cohort_end_date = observ_dates$observation_period_end_date
  )

  withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))

})

testthat::test_that("addbmi testing with sliding window",{
  cdm <- mockDrugExposure()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  cma_types <- c("CMA1", "CMA2", "CMA3")
  CMA_values <- calculateAdherenceSlidingWindowBatched(cdm = cdm, drugExposure = chronicDrugExposure, cma = cma_types)

  withBMI <- addBMI(CMA_values, cdm)

  testthat::expect_true(!is.null(withBMI))

})

testthat::test_that("testing summarisePatientCounts with cohort", {

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

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- calculateAdherenceBatched(drugExposure = chronicDrugExposure, cdm = cdm, cma = "CMA5")

  counts <- summarisePatientCounts(cdm = cdm, adherenceData = CMA_values, drugExposure = chronicDrugExposure, cohort = cohort_ref, cohortId = 1)

  testthat::expect_true(!is.null(counts))
})
