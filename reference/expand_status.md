# Expand regulations based on the management status field (e.g. seasonal, monthly recurring, etc.)

Function to create multiple rows of a regulation with distinct date
ranges that coincide with a recurring action (based on the management
status field). In this function, the management type needs to be
specified.

## Usage

``` r
expand_status(x, y)
```

## Arguments

- x:

  data default=fishhist

- y:

  Management Type Use

## Value

A dataframe

## Examples

``` r
expand_status(fishhist, "FISHING YEAR")
#> # A tibble: 7,079,973 × 49
#>    FMP          COMMON_NAME_CLEAN REGION ZONE_CLEAN SECTOR_CLEAN SUBSECTOR_CLEAN
#>    <chr>        <chr>             <chr>  <chr>      <chr>        <chr>          
#>  1 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        RECREATIONAL ALL            
#>  2 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        COMMERCIAL   ALL            
#>  3 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        RECREATIONAL ALL            
#>  4 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        COMMERCIAL   ALL            
#>  5 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        RECREATIONAL ALL            
#>  6 SHRIMP FISH… SHRIMP, NORTHERN… GULF … ALL        COMMERCIAL   ALL            
#>  7 SHRIMP FISH… SHRIMP, ROCK      GULF … ALL        RECREATIONAL ALL            
#>  8 SHRIMP FISH… SHRIMP, ROCK      GULF … ALL        COMMERCIAL   ALL            
#>  9 SHRIMP FISH… SHRIMP, ATLANIC … GULF … ALL        RECREATIONAL ALL            
#> 10 SHRIMP FISH… SHRIMP, ATLANIC … GULF … ALL        COMMERCIAL   ALL            
#> # ℹ 7,079,963 more rows
#> # ℹ 43 more variables: MANAGEMENT_TYPECLEAN <chr>, date_sequence <date>,
#> #   TIME_one <chr>, CLUSTER_one <int>, REG_ID_one <dbl>, FR_CITATION_one <chr>,
#> #   VALUE_one <chr>, VALUEUNITS_one <chr>, VALUETYPE_one <chr>,
#> #   VALUERATE_one <chr>, MULTI_REG_VALUE <dbl>, CLUSTER_weekly <dbl>,
#> #   REG_ID_weekly <dbl>, FR_CITATION_weekly <chr>, TIME_weekly <chr>,
#> #   VALUE_weekly <chr>, VALUEUNITS_weekly <chr>, VALUETYPE_weekly <chr>, …
```
