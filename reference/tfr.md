# Total Fertility Rate (TFR)

Total Fertility Rate (TFR)

## Usage

``` r
tfr(fx, age.int = 1)
```

## Arguments

- fx:

  Numeric array of age specific fertility rates.

- age.int:

  Numeric. Age group: `1` for one-year, `5` for five-year. Any age
  groups are allowed.

## Value

A length-1 numeric value equal to the sum of age-specific fertility
rates multiplied by `age.int`.

## Examples

``` r
fx <- c(0.02, 0.08, 0.12, 0.09, 0.04, 0.01, 0.001)
tfr(fx, age.int = 5)
#> [1] 1.805
```
