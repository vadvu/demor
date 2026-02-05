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

list with G0 (Gini coefficient), G0_abs (absolute Gini coefficient) and
data for the Lorenz curve (\$plot)
