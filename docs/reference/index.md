# Package index

### Data generation

Retrieves records from drug exposure table

- [`generateChronicDrugExposure()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/generateChronicDrugExposure.md)
  : Get drug exposure entries for adherence calculations

### Adherence calculations

Functions used to calculate medication adherence

- [`calculateAdherenceBatched()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/calculateAdherenceBatched.md)
  : Calculate adherence (batched processing)
- [`calculateAdherenceSlidingWindowBatched()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/calculateAdherenceSlidingWindowBatched.md)
  : Calculate adherence with sliding window approach (batched
  processing)
- [`calculateAdherence()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/calculateAdherence.md)
  : Calculate medication adherence
- [`calculateAdherenceSlidingWindow()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/calculateAdherenceSlidingWindow.md)
  : Calculate adherence using sliding window approach
- [`medicationGrouping()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/medicationGrouping.md)
  : Function for grouping medications

### Add features to data

Possibility to add cohorts as features and patient characteristics

- [`addCohorts()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/addCohorts.md)
  : Adds cohort columns to cma data
- [`addBMI()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/addBMI.md)
  : adds bmi values to CMA values

### Population stats

View statistics

- [`summarisePatientCounts()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/summarisePatientCounts.md)
  : Summarise patient counts

### Mockdata generation

Testing package, demos, internal functions

- [`mockDrugExposure()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/mockDrugExposure.md)
  : Mock database for testing package
- [`cleanNARows()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/cleanNARows.md)
  : Remove rows with NA CMA values and reset window IDs
- [`addPatientInformation()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/addPatientInformation.md)
  : Add patient information to CMA results
- [`loadData()`](https://healthinformaticsut.github.io/AdherenceFromOMOP/reference/loadData.md)
  : Load and prepare data for adherence calculations
