# e-dagger

e-dagger

## Usage

``` r
edagger(age, mx, ...)
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

A named numeric vector of the same length as `age`, where each element
is `e^\dagger_x`, the average remaining life years lost because of death
from age `x` onward.

## Examples

``` r
age <- 0:5
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
edagger(age, mx)[1:3]
#>        0        1        2 
#> 3.012988 2.342642 2.004861 
```
