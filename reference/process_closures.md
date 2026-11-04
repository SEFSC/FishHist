# Process ad connect all closure-related management types for a species within a region

Process closures to expand and incorporate all related mtypes for
closures to know for a single day whether the fishery is open or closed

## Usage

``` r
process_closures(data = fishhist, spp, region, start, end)
```

## Arguments

- data:

  default=fishhist

- spp:

  Filter to species of interest (common name).

- region:

  Filter to region of interest

## Value

A list

## Examples

``` r
process_closures(fishhist, spp = "SNAPPER, RED", region = "SOUTH ATLANTIC")
#> Error in dplyr::filter(data, COMMON_NAME_CLEAN == spp, REGION == region,     MANAGEMENT_TYPECLEAN %in% c("CLOSURE", "FISHING SEASON",         "FISHING YEAR", "PROHIBITED SALE AND PURCHASE", "PROHIBITED SPECIES"),     STARTDATE_CLEAN <= end, END_DATE >= start): ℹ In argument: `STARTDATE_CLEAN <= end`.
#> Caused by error:
#> ! argument "end" is missing, with no default
```
