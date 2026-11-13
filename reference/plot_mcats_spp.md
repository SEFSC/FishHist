# Plot of management categories recorded under an FMP for all managed species

Number of FRs published by management type and species.

## Usage

``` r
plot_mcats_spp(data = fishhist, FMP_filter)
```

## Arguments

- data:

  A dataframe (default=fishhist).

- FMP_filter:

  Filter to FMP of interest.

## Value

A ggplot object.

## Examples

``` r
plot_mcats_spp(fishhist,
    FMP_filter = "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION")

```
