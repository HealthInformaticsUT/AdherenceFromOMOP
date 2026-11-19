# Extracted from test-generateChronicDrugExposure.R:90

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "AdherenceFromOMOP", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cdm <- mockDrugExposure(nPerson = 5)
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
