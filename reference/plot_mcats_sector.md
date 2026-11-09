# Plot of management categories recorded under an FMP by sector

Percent of regulations by management type and sector (recreational vs.
commercial)

## Usage

``` r
plot_mcats_sector(data = fishhist, FMP_filter)
```

## Arguments

- data:

  A dataframe (default=fishhist)

- FMP_filter:

  Filter to FMP of interest

## Value

A ggplot object

## Examples

``` r
plot_mcats_sector(fishhist,
  FMP_filter = "REEF FISH RESOURCES OF THE GULF OF MEXICO")

```
