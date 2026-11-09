# Process and connect all closure-related management types for a species within a region

Process closures to expand and incorporate all related mtypes for
closures to know for a single day whether the fishery is open or closed

## Usage

``` r
process_closures(data = fishhist, spp, region, start, end)
```

## Arguments

- data:

  A dataframe (default=fishhist)

- spp:

  Filter to species of interest (common name).

- region:

  Filter to region of interest

## Value

A list where "data" is the full closure dataset (one row for each day
within the time series), "tab_sum_days_open" is a dataframe summarizing
the number of days the fishery is open within a strata (year, region,
zone, sector, subsector), "plot_days_open" is a ggplot plot object of
the number of days open by year within a strata, "tab_fr_dateranges" is
a dataframe of the start and end date ranges for the open and closed
seasons by region, zone, sector subsector, management type, and FR
citation, "heatmap_open" is a ggplot object of days open within a month
and year (NOTE: this plot applies to zone ALL), and "heatmap_closed" is
a ggplot object of the days closed within a month and year (NOTE: this
plot applies to zone ALL)

## Examples

``` r
process_closures(fishhist,
  spp = "SNAPPER, RED", region =
    "SOUTH ATLANTIC"
)
#> Error in dplyr::filter(data, COMMON_NAME_CLEAN == spp, REGION == region,     MANAGEMENT_TYPECLEAN %in% c("CLOSURE", "FISHING SEASON",         "FISHING YEAR", "PROHIBITED SALE AND PURCHASE", "PROHIBITED SPECIES"),     STARTDATE_CLEAN <= end, END_DATE >= start): ℹ In argument: `STARTDATE_CLEAN <= end`.
#> Caused by error:
#> ! argument "end" is missing, with no default
```
