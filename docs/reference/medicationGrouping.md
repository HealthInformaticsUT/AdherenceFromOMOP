# Function for grouping medications

Uses CodelistGenerator to get drug ingredient codes

## Usage

``` r
medicationGrouping(cdm, ingredientGroups = NULL)
```

## Arguments

- cdm:

  (\`cdm_reference\`) A CDM reference object created with CDMConnector.

- ingredientGroups:

  previously determined grouping, format as list(name = c(ingredient,
  ingredient)). Ingredient can be marked as concept ID or ingredient
  name.

## Value

codelist generator output with the structure of ingredientGroups

## Examples

``` r
if (FALSE) { # \dontrun{

 mock_cdm <- CodelistGenerator::mockVocabRef()
groups <- medicationGrouping(mock_cdm, ingredientGroups=list(group1 = c("Adalimumab")))
} # }
```
