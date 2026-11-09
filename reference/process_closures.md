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

- start:

  Process all closure-related management types starting from a defined
  start date (formatted using \`as.Date\`)

- end:

  Process all closure-related management types starting from a defined
  end date (formatted using \`as.Date\`)

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
  spp = "SNAPPER, RED", region = "SOUTH ATLANTIC",
  start = "2000-01-01", end = "2025-12-31"
)
#> $data
#> # A tibble: 18,994 × 11
#>    FMP          COMMON_NAME_CLEAN REGION ZONE_CLEAN SECTOR_CLEAN SUBSECTOR_CLEAN
#>    <chr>        <chr>             <chr>  <chr>      <chr>        <chr>          
#>  1 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  2 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  3 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  4 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  5 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  6 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  7 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  8 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  9 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#> 10 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#> # ℹ 18,984 more rows
#> # ℹ 5 more variables: date_sequence <date>, MANAGEMENT_TYPECLEAN <chr>,
#> #   FR_CITATION <chr>, TIME <chr>, VALUE <chr>
#> 
#> $tab_sum_days_open
#> # A tibble: 52 × 8
#>    FMP    COMMON_NAME_CLEAN REGION ZONE_CLEAN SECTOR_CLEAN SUBSECTOR_CLEAN  YEAR
#>    <chr>  <chr>             <chr>  <chr>      <chr>        <chr>           <dbl>
#>  1 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2000
#>  2 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2001
#>  3 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2002
#>  4 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2003
#>  5 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2004
#>  6 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2005
#>  7 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2006
#>  8 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2007
#>  9 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2008
#> 10 SNAPP… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL              2009
#> # ℹ 42 more rows
#> # ℹ 1 more variable: Days_Open <int>
#> 
#> $plot_days_open

#> 
#> $tab_fr_dateranges
#> # A tibble: 27 × 11
#>    FMP          COMMON_NAME_CLEAN REGION ZONE_CLEAN SECTOR_CLEAN SUBSECTOR_CLEAN
#>    <chr>        <chr>             <chr>  <chr>      <chr>        <chr>          
#>  1 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  2 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  3 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  4 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  5 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  6 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  7 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  8 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#>  9 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#> 10 SNAPPER-GRO… SNAPPER, RED      SOUTH… ALL        COMMERCIAL   ALL            
#> # ℹ 17 more rows
#> # ℹ 5 more variables: FR_CITATION <chr>, MANAGEMENT_TYPECLEAN <chr>,
#> #   episode_id <int>, START <date>, END <date>
#> 
#> $heatmap_open

#> 
#> $heatmap_closed

#> 
```
