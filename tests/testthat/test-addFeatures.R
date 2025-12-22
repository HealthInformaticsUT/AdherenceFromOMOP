testthat::test_that("testing countPeople", {

  cdm <- mockAdherenceFromOMOP()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure,cdm = cdm,cma = "CMA5")

  counts <- countPeople(cdm= cdm, cmaTable = CMA_values, generatedTable = chronicDrugExposure)

  testthat::expect_true(!is.null(counts))
})


testthat::test_that("addBMI testing",{
  cdm <- mockAdherenceFromOMOP()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure,cdm = cdm,cma = "CMA5")

  withBMI <- addBMI(CMA_values, cdm)

  testthat::expect_true(!is.null(withBMI))

})

testthat::test_that("addCohorts testing",{
  cdm <- mockAdherenceFromOMOP()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure,cdm = cdm,cma = "CMA5")

  cohortSet <- CDMConnector::readCohortSet(paste0(getwd(), "/inst"))
  cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "cohort")

  cohortTable <- dplyr::collect(cdm$cohort)

  withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))

})

testthat::test_that("addCohorts testing",{
  cdm <- mockAdherenceFromOMOP()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  cma_types <- c("CMA1", "CMA2", "CMA3")
  CMA_values <- adherenceSlidingWindowLoader(cdm = cdm, reference = chronicDrugExposure, cma = cma_types)

  cohortSet <- CDMConnector::readCohortSet(paste0(getwd(), "/inst"))
  cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "cohort")

  cohortTable <- dplyr::collect(cdm$cohort)

  withCohort <- addCohorts(CMA_values, cohortTable)

  testthat::expect_true(!is.null(withCohort))

})

testthat::test_that("addbmi testing with sliding window",{
  cdm <- mockAdherenceFromOMOP()

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  cma_types <- c("CMA1", "CMA2", "CMA3")
  CMA_values <- adherenceSlidingWindowLoader(cdm = cdm, reference = chronicDrugExposure, cma = cma_types)

  withBMI <- addBMI(CMA_values, cdm)

  testthat::expect_true(!is.null(withBMI))

})

testthat::test_that("testing countPeople", {

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

  chronicDrugExposure <- generateChronicDrugExposure(cdm = cdm, name = "chronic_drug_exposure_table", overwrite = T)

  CMA_values <- adherenceCalculationsLoader(reference = chronicDrugExposure,cdm = cdm,cma = "CMA5")

  counts <- countPeople(cdm= cdm, cmaTable = CMA_values, generatedTable = chronicDrugExposure, cohortSchema = "main", cohortId = 1)

  testthat::expect_true(!is.null(counts))
})
