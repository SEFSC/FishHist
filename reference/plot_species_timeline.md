# Timeline visual

Timeline of management actions publised for a species within a FMP

## Usage

``` r
plot_species_timeline(data = fishhist, spp, region)
```

## Arguments

- data:

  default=fishhist

- spp:

  Filter to species of interest (common name).

- region:

  Filter to region of interest

## Value

A plot

## Examples

``` r
plot_species_timeline(fishhist, spp = "SNAPPER, RED", region = "SOUTH ATLANTIC")

```
