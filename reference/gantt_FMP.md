# Gantt chart of FMPs

Gantt chart of the first and last Federal Register (FR) published within
a Fishery Management Plan (FMP) within the Southeast Fisheries
Management History dataset.

## Usage

``` r
gantt_FMP(data = fishhist)
```

## Arguments

- data:

  A dataframe (default=fishhist)

## Value

A ggplot object

## Examples

``` r
gantt_FMP(fishhist)
#> Warning: no non-missing arguments to min; returning Inf
#> Error in seq.default(min(data_summary$start_year, na.rm = TRUE), max(data_summary$end_year,     na.rm = TRUE), by = 5): 'from' must be a finite number
```
