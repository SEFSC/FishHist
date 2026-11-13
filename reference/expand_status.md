# Expand regulations based on the management status field (e.g. seasonal, monthly recurring, etc.)

Function to create multiple rows of a regulation with distinct date
ranges that coincide with a recurring action (based on the management
status field). In this function, the management type needs to be
specified.

## Usage

``` r
expand_status(data = fishhist, y)
```

## Arguments

- data:

  A dataframe (default=fishhist).

- y:

  Management type.

## Value

A dataframe where the management status is expanded for the selected
management type and each represents a day within a time series. The
status is formatted into wide to view how regulations with differing
status types are overwritten depending on the FR Citation.

The additional columns include the following. The fields in bold are the
most important indicating the rule in effect for each date:

|  |  |  |
|----|----|----|
| *Field* | *Data Type* | *Description* |
| date_sequence | Date | Date within time series |
| TIME_one | Chr | Time for regulation with status ONCE |
| CLUSTER_one | Int | Cluster ID for regulation with status ONCE |
| REG_ID_one | Num | Regulation ID for regulation with status ONCE |
| FR_CITATION_one | Chr | FR Citation for regulation with status ONCE |
| VALUE_one | Chr | Value for regulation with status ONCE |
| VALUEUNITS_one | Chr | Value units for regulation with status ONCE |
| VALUETYPE_one | Chr | Value type for regulation with status ONCE |
| VALUERATE_one | Chr | Value rate for regulation with status ONCE |
| TIME_weekly | Chr | Time for regulation with status WEEKLY |
| CLUSTER_weekly | Int | Cluster ID for regulation with status WEEKLY |
| REG_ID_weekly | Num | Regulation ID for regulation with status WEEKLY |
| FR_CITATION_weekly | Chr | FR Citation for regulation with status WEEKLY |
| VALUE_weekly | Chr | Value for regulation with status WEEKLY |
| VALUEUNITS_weekly | Chr | Value units for regulation with status WEEKLY |
| VALUETYPE_weekly | Chr | Value type for regulation with status WEEKLY |
| VALUERATE_weekly | Chr | Value rate for regulation with status WEEKLY |
| TIME_seasonal | Chr | Time for regulation with status SEASONAL |
| CLUSTER_seasonal | Int | Cluster ID for regulation with status SEASONAL |
| REG_ID_seasonal | Num | Regulation ID for regulation with status SEASONAL |
| FR_CITATION_seasonal | Chr | FR Citation for regulation with status SEASONAL |
| VALUE_seasonal | Chr | Value for regulation with status SEASONAL |
| VALUEUNITS_seasonal | Chr | Value units for regulation with status SEASONAL |
| VALUETYPE_seasonal | Chr | Value type for regulation with status SEASONAL |
| VALUERATE_seasonal | Chr | Value rate for regulation with status SEASONAL |
| TIME_monthly | Chr | Time for regulation with status MONTHLY |
| CLUSTER_monthly | Int | Cluster ID for regulation with status MONTHLY |
| REG_ID_monthly | Num | Regulation ID for regulation with status MONTHLY |
| FR_CITATION_monthly | Chr | FR Citation for regulation with status MONTHLY |
| VALUE_monthly | Chr | Value for regulation with status MONTHLY |
| VALUEUNITS_monthly | Chr | Value units for regulation with status MONTHLY |
| VALUETYPE_monthly | Chr | Value type for regulation with status MONTHLY |
| VALUERATE_monthly | Chr | Value rate for regulation with status MONTHLY |
| **FR_CITATION** | Chr | Maximum FR Citation across all status types (ONCE, WEEKLY, SEASONAL, and MONTHLY). The maximum FR Citation indicates the most recent FR |
| **TIME** | Chr | Time that corresponds to the maximum FR Citation |
| **VALUE** | Chr | Value that correspond to the maximum FR Citation |
| **VALUEUNITS** | Chr | Value units that correspond to the maximum FR Citation |
| **VALUETYPE** | Chr | Value type that correspond to the maximum FR Citation |
| **VALUERATE** | Chr | Value rate that correspond to the maximum FR Citation |

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
