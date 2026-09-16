# Get drug exposure entries for adherence calculations

Get drug exposure entries for adherence calculations

## Usage

``` r
generateChronicDrugExposure(
  cdm,
  name = "adherenceFromOMOP",
  conceptSet = NULL,
  indexDate = "drug_exposure_start_date",
  overwrite = FALSE,
  subjects = NULL,
  cohort = NULL,
  cohortId = NULL,
  ...
)
```

## Arguments

- cdm:

  (\`cdm_reference\`) A CDM reference object created with CDMConnector.

- name:

  (\`character(1)\`) Name for the output table in the database.

- conceptSet:

  (\`list\`, \`codelist\`, or \`NULL\`) Named list of drug concept IDs.
  Recommended to use \[CodelistGenerator::getDrugIngredientCodes()\]. If
  \`NULL\`, all drugs in the drug_exposure table are included.

- indexDate:

  (\`character(1)\`) Name of the column to use as the index date for
  patient-level calculations. Default: \`"drug_exposure_start_date"\`.

- overwrite:

  If table with provided name exists, should it be overwritten? Default:
  \`FALSE\`.

- subjects:

  (\`numeric\` or \`NULL\`) Optional vector of person_id values to
  restrict the drug exposure data to specific subjects.

- cohort:

  (\`tbl\` or \`NULL\`) Optional cohort table reference to filter drug
  exposures. Only drug exposures for subjects in the cohort and within
  the cohort period will be included.

- cohortId:

  (\`numeric\` or \`NULL\`) If \`cohort\` is provided, optional vector
  of cohort_definition_id values to use. If \`NULL\`, all cohorts in the
  table are used.

- ...:

  Additional arguments (currently unused).

## Value

A database table reference containing drug exposure records with
columns: person_id, drug_concept_id, drug_exposure_start_date,
days_supply, date_of_birth, sex, date_of_death,
observation_period_start_date, observation_period_end_date. When
cohortSchema is used, also includes cohort_start_date and
cohort_end_date. Returns NULL if validation fails or no records match
the criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
cdm <- mockDrugExposure()
conceptSet <- CodelistGenerator::getDrugIngredientCodes(cdm, name = "Atorvastatin")

drugExposure <- generateChronicDrugExposure(
  cdm = cdm,
  conceptSet = conceptSet,
  name = "chronic_drug_exposure",
  overwrite = TRUE
)
} # }
```
