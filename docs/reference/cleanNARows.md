# Remove rows with NA CMA values and reset window IDs

Filters out rows where CMA calculation returned NA and resets window.ID
numbering to be sequential within each contiguous block of non-NA
results. This handles gaps in sliding window calculations where some
windows may not have sufficient data for CMA computation.

## Usage

``` r
cleanNARows(data)
```

## Arguments

- data:

  Data frame containing CMA results with columns: name (CMA type),
  person_id, group, CMA, and optionally window.ID for sliding window
  results.

## Value

Data frame with NA CMA rows removed. For sliding window data, window.ID
is reset to start from 1 within each contiguous series of non-NA values,
grouped by name, person_id, and group.
