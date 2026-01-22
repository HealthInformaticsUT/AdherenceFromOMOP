# DARWIN EU Alignment Issues and Recommendations

**Package:** AdherenceFromOMOP
**Analysis Date:** 2026-01-08
**Reference Package:** DrugUtilisation (DARWIN EU)

This document outlines inconsistencies between AdherenceFromOMOP and DARWIN EU coding conventions for OMOP CDM packages, with specific recommendations for alignment.

---

## Important Note on Package Architecture

AdherenceFromOMOP works with **drug exposure data**, not cohorts. The key distinction:

- **`generateChronicDrugExposure()`** creates an enriched drug exposure table with multiple rows per person (one per prescription/dispensing event)
- This is fundamentally different from a `cohort_table` which has one row per person per cohort entry
- Adherence calculations operate on this drug exposure data structure
- The output is adherence metrics, not cohort membership

Therefore, recommendations around `cohort_table` S3 classes, cohort attrition tracking, and `summarised_result` return types from other DARWIN EU packages **do not directly apply** to this package's core functionality.

---

## 1. Function Naming Conventions

### Current State

**Consistent patterns:**
- camelCase used throughout
- `generate*` prefix for data generation
- `add*` prefix for feature enrichment

**Inconsistencies:**

| Current Function | Issue | Recommendation |
|-----------------|-------|----------------|
| `adherenceCalculations()` | Noun-based, unclear verb | `calculateAdherence()` |
| `adherenceCalculationsLoader()` | "Loader" suffix non-standard | `calculateAdherenceBatched()` or make internal |
| `adherenceSlidingWindow()` | Noun-based | `calculateAdherenceSlidingWindow()` |
| `adherenceSlidingWindowLoader()` | "Loader" suffix | `calculateAdherenceSlidingWindowBatched()` or make internal |
| `countPeople()` | Vague, doesn't follow pattern | `summarisePatientCounts()` |
| `mockAdherenceFromOMOP()` | Redundant package name | `mockDrugExposure()` |

### Recommended Naming

```r
# Data Generation
generateChronicDrugExposure()      # ✓ Keep as-is (accurate name)

# Adherence Calculations
calculateAdherence()               # Clear verb-noun pattern
calculateAdherenceSlidingWindow()  # Clear purpose

# Feature Enrichment
addBMI()                           # ✓ Keep as-is
addCohortFlags()                   # More descriptive than addCohorts()

# Summary Statistics
summarisePatientCounts()           # Follows summarise* pattern

# Testing
mockDrugExposure()                 # Shorter, descriptive
```

**Note:** The "Loader" functions could be made internal, with batching handled automatically based on data size, or kept as explicit user choices with clearer names.

---

## 2. Parameter Naming Standards

### Issue: Non-standard Parameter Names

**DARWIN EU uses standardized names:**
- `conceptSet` for lists of concept IDs (not `drugConceptIdList`)
- `name` for output table names (not `resultsTableName`)
- Short, clear names for date columns (not `indexDateForPatientCalculation`)

### Recommended Changes

| Current Parameter | Standard Name | Rationale |
|-------------------|---------------|-----------|
| `drugConceptIdList` | `conceptSet` | DARWIN EU standard |
| `resultsTableName` | `name` | Consistent with other packages |
| `indexDateForPatientCalculation` | `indexDate` | Too verbose |
| `cma_table` | `adherenceData` or `drugExposure` | More accurate than snake_case `cma_table` |
| `cmaTable` | `adherenceData` | Consistent with above |
| `generatedTable` | `drugExposure` | More specific |

### Example Changes

```r
# BEFORE
generateChronicDrugExposure(
  cdm,
  name = "adherenceFromOMOP",
  indexDateForPatientCalculation = "drug_exposure_start_date",
  drugConceptIdList = NULL,
  ...
)

# AFTER
generateChronicDrugExposure(
  cdm,
  name = "adherenceFromOMOP",
  indexDate = "drug_exposure_start_date",
  conceptSet = NULL,
  ...
)

# BEFORE
adherenceCalculationsLoader(
  reference,
  cdm,
  resultsTableName = "adherenceFromOMOP_results",
  cma = c("CMA5")
)

# AFTER
calculateAdherence(
  drugExposure,
  cdm,
  name = "adherenceFromOMOP_results",
  cma = c("CMA5")
)
```

**Files to modify:**
- `R/generateChronicDrugExposure.R`: `drugConceptIdList` → `conceptSet`, `indexDateForPatientCalculation` → `indexDate`
- `R/adherenceCalculations.R`: `resultsTableName` → `name`, `reference` → `drugExposure`
- `R/addFeatures.R`: `cma_table` → consistent camelCase

---

## 3. Parameter Naming Consistency (snake_case vs camelCase)

### Issue: Mixed Conventions

```r
addBMI(cma_table, cdm)              # snake_case parameter
addCohorts(cmaTable, cohortTable)   # camelCase parameter
countPeople(cmaTable = NULL)        # camelCase parameter
```

### Recommendation

Use **camelCase consistently** throughout:

```r
# BEFORE
addBMI <- function(cma_table, cdm)

# AFTER
addBMI <- function(adherenceData, cdm)
```

**Files to modify:**
- `R/addFeatures.R` line 10: `cma_table` → `adherenceData`

---

## 4. Parameter Order Patterns

### Issue: Inconsistent Ordering

| Function | Current Order | Pattern |
|----------|---------------|---------|
| `generateChronicDrugExposure()` | `(cdm, name, ...)` | CDM first |
| `adherenceCalculationsLoader()` | `(reference, cdm, ...)` | Data first |
| `adherenceCalculations()` | `(cdm, data, cma, ...)` | CDM first |
| `addBMI()` | `(cma_table, cdm)` | Data first |
| `countPeople()` | `(cdm, cmaTable, ...)` | CDM first |

### Recommendation

Adopt a consistent pattern based on function purpose:

**For data transformation functions (add*, calculate*):**
- Data first (enables piping)

**For data generation functions (generate*):**
- CDM first (creates new data in CDM)

```r
# Data generation - CDM first
generateChronicDrugExposure(cdm, name, conceptSet, ...)

# Data transformation - data first (pipeable)
calculateAdherence(drugExposure, cdm, cma, name, ...)
addBMI(adherenceData, cdm, ...)

# Summary functions - data first (pipeable)
summarisePatientCounts(adherenceData, drugExposure, cdm, ...)
```

**Files to modify:**
- `R/adherenceCalculations.R`: Reorder to `(drugExposure, cdm, ...)`
- `R/addFeatures.R` line 226 (`countPeople`): Reorder to `(adherenceData, drugExposure, cdm, ...)`

---

## 5. Database Parameter Abstraction

### Issue: Exposed Database Structure

Current API exposes database-specific parameters:

```r
generateChronicDrugExposure(
  cohortSchema = NULL,      # DB schema - should be internal
  cohortTableName = "cohort" # DB table name - should be internal
)
```

### Recommendation

Use CDM table references instead of schema/table names:

```r
# BEFORE
generateChronicDrugExposure(
  cdm,
  cohortSchema = "my_schema",
  cohortTableName = "my_cohort",
  cohortId = 1
)

# AFTER
generateChronicDrugExposure(
  cdm,
  cohort = cdm$my_cohort,  # Pass cohort table directly
  cohortId = 1
)

# Or even simpler - just use cohortId if cohort is always from cdm
generateChronicDrugExposure(
  cdm,
  cohortId = 1,            # Uses cdm$cohort by default
  cohortName = "my_cohort" # Optional: specify different cohort table in cdm
)
```

**Benefits:**
- Users don't need to know database schema names
- Works seamlessly with CDMConnector patterns
- Easier to use and less error-prone

**Files to modify:**
- `R/generateChronicDrugExposure.R`: Replace `cohortSchema`, `cohortTableName` with `cohort` or `cohortName` parameter

---

## 6. Validation Patterns

### Issue: Manual Validation with Early Returns

Current approach:
```r
if (is.null(reference)) {
  cli::cli_alert_warning("Provided reference is faulty")
  return(NULL)
}

if (!all(cma %in% c("CMA1", ...))) {
  cli::cli_alert_danger("Only CMAs 1-9 are expected.")
  return(NULL)
}
```

### Recommendation

Use omopgenerics assertion functions where applicable:

```r
# For CDM validation
cdm <- omopgenerics::validateCdmArgument(cdm)

# For simple assertions
omopgenerics::assertCharacter(name, length = 1)
omopgenerics::assertNumeric(batchSize, integerish = TRUE, min = 1)
omopgenerics::assertChoice(cma, choices = paste0("CMA", 1:9), length = NULL)
omopgenerics::assertLogical(overwrite, length = 1)

# For table validation
omopgenerics::assertTable(
  drugExposure,
  columns = c("person_id", "drug_concept_id", "drug_exposure_start_date", "days_supply")
)
```

**Benefits:**
- Consistent error messages across DARWIN EU packages
- Better error handling (throws errors, doesn't return NULL)
- Standardized validation patterns

**Note:** Continue to return `NULL` in cases where it's a valid outcome (e.g., empty results after filtering), but use assertions for invalid inputs.

**Files to modify:**
- All R files with exported functions: Add omopgenerics validation

---

## 7. Documentation Patterns

### Issue: Repetitive Inline Documentation

Each function has complete parameter documentation, leading to inconsistencies:
```r
#' @param cdm a cdm_reference object
#' @param cdm A cdm reference via CDMConnector.
#' @param cdm A cdm reference object
```

### Recommendation

Create documentation helper file with `@inheritParams`:

**Create `R/documentationHelpers.R`:**

```r
#' Documentation helper for cdm parameter
#' @param cdm (`cdm_reference`) A CDM reference object created with CDMConnector.
#' @name cdmDoc
#' @keywords internal
NULL

#' Documentation helper for conceptSet parameter
#' @param conceptSet (`list` or `codelist` or `NULL`) Named list of drug concept IDs.
#'   Recommended to use [CodelistGenerator::getDrugIngredientCodes()].
#'   If `NULL`, all drugs in the drug_exposure table are included.
#' @name conceptSetDoc
#' @keywords internal
NULL

#' Documentation helper for name parameter
#' @param name (`character(1)`) Name for the output table in the database.
#' @name nameDoc
#' @keywords internal
NULL

#' Documentation helper for cma parameter
#' @param cma (`character`) Vector of CMA types to calculate. Options:
#'   `"CMA1"` through `"CMA9"`. See [AdhereR documentation](https://www.adherer.eu/)
#'   for details on each CMA type.
#' @name cmaDoc
#' @keywords internal
NULL

#' Documentation helper for drugExposure parameter
#' @param drugExposure Database table reference or data frame containing drug exposure
#'   data, typically generated by [generateChronicDrugExposure()].
#' @name drugExposureDoc
#' @keywords internal
NULL
```

**Use in functions:**
```r
#' Calculate medication adherence
#'
#' Calculates CMA (Continuous Medication Availability) metrics for drug exposure data.
#'
#' @inheritParams drugExposureDoc
#' @inheritParams cdmDoc
#' @inheritParams cmaDoc
#' @inheritParams nameDoc
#'
#' @return Data frame with adherence metrics per person and medication group.
#' @export
calculateAdherence <- function(drugExposure, cdm, cma = paste0("CMA", 1:9), name = NULL, ...) {
```

**Files to create:**
- `R/documentationHelpers.R` (new file)

**Files to modify:**
- All R files with exported functions: Use `@inheritParams`

---

## 8. Piping Support

### Issue: Limited Pipe Compatibility

Some functions are pipe-friendly, others are not:

```r
# Current workflow (not very pipeable)
drugExposure <- generateChronicDrugExposure(cdm, name = "exposure")
adherence <- adherenceCalculationsLoader(drugExposure, cdm, ...)
withBMI <- addBMI(adherence, cdm)
```

### Recommendation

Ensure calculation and enrichment functions are pipe-friendly:

```r
# Improved workflow
drugExposure <- generateChronicDrugExposure(cdm, name = "exposure", conceptSet = drugs)

result <- drugExposure |>
  calculateAdherence(cdm, cma = c("CMA5", "CMA7")) |>
  addBMI(cdm) |>
  addCohortFlags(cdm$diabetes_cohort)
```

**Key changes needed:**
1. Calculation functions take drug exposure as first argument
2. Add functions take adherence data as first argument
3. All return the modified data for chaining

---

## 9. Return Value Consistency

### Current Patterns

| Function | Returns | Appropriate? |
|----------|---------|--------------|
| `generateChronicDrugExposure()` | Database table reference | ✓ Yes |
| `adherenceCalculations()` | Data frame | ✓ Yes |
| `adherenceCalculationsLoader()` | Database table reference | ✓ Yes |
| `addBMI()` | Data frame (forces collection) | ⚠ Could maintain lazy eval |
| `countPeople()` | Named list | ✓ Yes |

### Recommendation: Avoid Forced Collection

Current `addBMI()` forces data to memory:
```r
addBMI <- function(cma_table, cdm) {
  cma_table <- dplyr::collect(cma_table)  # Forces to memory
  ...
}
```

Consider maintaining lazy evaluation when possible:
```r
addBMI <- function(adherenceData, cdm, collect = FALSE) {
  # Perform joins using dplyr (works with DB references)
  result <- adherenceData |>
    dplyr::left_join(...)

  if (collect) {
    result <- dplyr::collect(result)
  }

  return(result)
}
```

**Note:** This may not always be possible depending on AdhereR requirements.

---

## 10. Missing Parameter in Loader Functions

### Issue: Inconsistent `medicationGroup` Parameter

```r
# adherenceSlidingWindowLoader HAS medicationGroup (line 28)
adherenceSlidingWindowLoader <- function(reference, cdm, ..., medicationGroup = NULL, ...)

# adherenceCalculationsLoader MISSING medicationGroup (line 228)
adherenceCalculationsLoader <- function(reference, cdm, ..., batchSize = 1000, ...)
# medicationGroup only available through ...
```

### Recommendation

Add explicit `medicationGroup` parameter to `adherenceCalculationsLoader()`:

```r
calculateAdherenceBatched <- function(
  drugExposure,
  cdm,
  name = "adherenceFromOMOP_results",
  cma = paste0("CMA", 1:9),
  medicationGroup = NULL,  # Add explicit parameter
  batchSize = 1000,
  ...
) {
```

**Files to modify:**
- `R/adherenceCalculations.R` line 228: Add `medicationGroup = NULL` parameter

---

## 11. Implementation Priority

### Phase 1: Non-Breaking Improvements (v0.2.0)

**Can be done without breaking existing API:**

1. **Create documentation helpers** (`R/documentationHelpers.R`)
2. **Add parameter aliases** (accept both old and new names during transition)
3. **Add omopgenerics validation** alongside existing checks
4. **Add `medicationGroup` to `adherenceCalculationsLoader()`**

### Phase 2: API Refinements (v1.0.0)

**Breaking changes with deprecation warnings:**

1. **Rename functions:**
   - `adherenceCalculations()` → `calculateAdherence()`
   - `countPeople()` → `summarisePatientCounts()`
   - `mockAdherenceFromOMOP()` → `mockDrugExposure()`

2. **Rename parameters:**
   - `drugConceptIdList` → `conceptSet`
   - `resultsTableName` → `name`
   - `indexDateForPatientCalculation` → `indexDate`
   - `cma_table` → `adherenceData`

3. **Reorder parameters** for pipe-friendliness

4. **Simplify database parameters:**
   - Replace `cohortSchema`/`cohortTableName` with `cohort` or `cohortName`

### Phase 3: Polish (v1.1.0)

1. **Improve lazy evaluation** in add* functions
2. **Consider internal batching** (remove Loader functions from public API)
3. **Enhanced error messages**

---

## 12. Summary of Recommendations

### Keep As-Is (Already Good)

- `generateChronicDrugExposure()` function name (accurate)
- `addBMI()` function name
- Return type of `generateChronicDrugExposure()` (table reference)
- Return type of adherence calculations (data frame with CMA results)
- Overall package structure and workflow

### Change (Parameter Names)

| Current | Recommended | Priority |
|---------|-------------|----------|
| `drugConceptIdList` | `conceptSet` | HIGH |
| `resultsTableName` | `name` | HIGH |
| `indexDateForPatientCalculation` | `indexDate` | MEDIUM |
| `cma_table` | `adherenceData` | MEDIUM |
| `cohortSchema` + `cohortTableName` | `cohort` or `cohortName` | MEDIUM |

### Change (Function Names)

| Current | Recommended | Priority |
|---------|-------------|----------|
| `adherenceCalculations()` | `calculateAdherence()` | MEDIUM |
| `adherenceSlidingWindow()` | `calculateAdherenceSlidingWindow()` | MEDIUM |
| `countPeople()` | `summarisePatientCounts()` | LOW |
| `mockAdherenceFromOMOP()` | `mockDrugExposure()` | LOW |

### Add

| Addition | Priority |
|----------|----------|
| `medicationGroup` to `adherenceCalculationsLoader()` | HIGH |
| Documentation helpers file | MEDIUM |
| omopgenerics validation | MEDIUM |
| Deprecation warnings for renamed items | HIGH (when renaming) |

---

## 13. Things NOT Recommended (Clarification)

The following suggestions from the initial analysis **do not apply** to this package:

1. ~~Returning `cdm` object from `generateChronicDrugExposure()`~~ - Current approach (returning table reference) is correct for drug exposure data

2. ~~Using `cohort_table` S3 class~~ - Drug exposure data is not a cohort; it has multiple rows per person

3. ~~Using `omopgenerics::newCohortTable()`~~ - Not applicable to drug exposure data

4. ~~Using `omopgenerics::recordCohortAttrition()`~~ - Cohort attrition doesn't apply to drug exposure records

5. ~~Returning `summarised_result` from adherence calculations~~ - Adherence results have a different structure; the current data frame return is appropriate

6. ~~Renaming to `generateAdherenceCohort()`~~ - This creates drug exposure data, not a cohort

---

## 14. Quick Reference: Before and After

### Parameter Changes Summary

```r
# generateChronicDrugExposure
drugConceptIdList          →  conceptSet
indexDateForPatientCalculation  →  indexDate
cohortSchema + cohortTableName  →  cohort (or cohortName)

# adherenceCalculationsLoader / calculateAdherence
resultsTableName           →  name
reference                  →  drugExposure
(add medicationGroup explicitly)

# addBMI
cma_table                  →  adherenceData

# countPeople / summarisePatientCounts
cmaTable                   →  adherenceData
generatedTable             →  drugExposure
```

### Workflow Comparison

```r
# CURRENT
cdm <- mockAdherenceFromOMOP()
drugCodes <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")
concepts <- unlist(drugCodes, use.names = FALSE)

drugExposure <- generateChronicDrugExposure(
  cdm = cdm,
  drugConceptIdList = concepts,
  name = "drug_exposure",
  indexDateForPatientCalculation = "drug_exposure_start_date"
)

adherence <- adherenceCalculationsLoader(
  reference = drugExposure,
  cdm = cdm,
  resultsTableName = "adherence_results",
  cma = c("CMA5")
)

withBMI <- addBMI(adherence, cdm)

# RECOMMENDED
cdm <- mockDrugExposure()
conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")

drugExposure <- generateChronicDrugExposure(
  cdm = cdm,
  conceptSet = conceptSet,
  name = "drug_exposure",
  indexDate = "drug_exposure_start_date"
)

result <- drugExposure |>
  calculateAdherence(cdm, cma = c("CMA5"), name = "adherence_results") |>
  addBMI(cdm)
```

---

**Document Version:** 2.0
**Last Updated:** 2026-01-08
**Status:** Revised based on package architecture review
