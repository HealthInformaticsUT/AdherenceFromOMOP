# Mock database for testing package

Creates a mock CDM database with drug exposure data for testing and
examples.

## Usage

``` r
mockDrugExposure(nPerson = 5, recordPerson = 3, dbms = duckdb::duckdb())
```

## Arguments

- nPerson:

  (\`numeric(1)\`) Number of persons to generate in the mock database.
  Default: 5.

- recordPerson:

  (\`numeric(1)\`) Number of drug exposure records per person. Default:
  3.

- dbms:

  Database management system driver. Default: \`duckdb::duckdb()\`.

## Value

A \`cdm_reference\` object with mock tables including person,
drug_exposure, observation_period, measurement, and death tables.

## Examples

``` r
if (FALSE) { # \dontrun{
cdm <- mockDrugExposure()
} # }
```
