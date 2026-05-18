# Gini coefficient of a life table

Gini coefficient of a life table

## Usage

``` r
gini(age, mx, ...)
```

## Arguments

- age:

  Numeric array of age intervals

- mx:

  Numeric array with age specific mortality rates.

- ...:

  Optional. Additional arguments for
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) function.

## Value

A list with two components: `Gini`, itself a list with the relative Gini
coefficient `G0` and the absolute Gini coefficient `G0_abs`; and `plot`,
a data frame with columns `Fx` and `Phix` for drawing the Lorenz curve.

## Examples

``` r
age <- 0:5
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
gini(age, mx)$Gini
#> $G0
#>         ex 
#> 0.06904865 
#> 
#> $G0_abs
#>       ex 
#> 2.464346 
#> 
```
