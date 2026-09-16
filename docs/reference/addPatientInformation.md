# Add patient information to CMA results

Enriches CMA calculation results with patient demographic and
observation period data.

## Usage

``` r
addPatientInformation(data, cma_values)
```

## Arguments

- data:

  Data frame containing drug exposure records with patient information,
  typically output from generateChronicDrugExposure.

- cma_values:

  Data frame containing CMA calculation results from AdhereR functions.

## Value

Data frame combining CMA results with patient information.
