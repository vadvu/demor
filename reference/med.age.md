# Median age calculation

Median age calculation

## Usage

``` r
med.age(N, age)
```

## Arguments

- N:

  Numeric array. Population counts by age groups (from young to old)

- age:

  Numeric array. Lower bounds of age groups, same length as `N`

## Value

A length-1 numeric value giving the estimated median age of the
population represented by `N`.

## Examples

``` r
N <- c(100, 90, 80, 70, 60)
age <- seq(0, 20, 5)
med.age(N, age)
#> [1] 10.62
```
