# Split a medication group's data by administration route

Further stratifies a medication group's drug exposure records by
\`route_concept_id\`, on top of the general ingredient-level grouping.
Records with a missing or zero \`route_concept_id\` remain part of the
general ingredient-level group rather than being dropped or split out.

## Usage

``` r
splitGroupByRoute(data, groupName)
```

## Arguments

- data:

  Data frame of drug exposure records already filtered to a single
  medication group.

- groupName:

  (\`character(1)\`) Name of the ingredient-level medication group.

## Value

Named list of data frames: one per route found in \`data\` (named
\`"groupName_routeroute_concept_id"\`), plus one named \`groupName\`
containing records without a route concept id (only present if such
records exist).
