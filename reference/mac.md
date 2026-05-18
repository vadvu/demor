# Mean Age at Childbearing (MAC)

Mean Age at Childbearing (MAC)

## Usage

``` r
mac(fx, age)
```

## Arguments

- fx:

  Numeric array of age specific fertility rates.

- age:

  Numeric array of ages. For example, `15:55` for 1-year age-groups

## Value

A length-1 numeric value giving the mean age at childbearing implied by
`fx`.

## Examples

``` r
age <- seq(15, 45, 5)
fx <- c(0.03, 0.10, 0.14, 0.12, 0.07, 0.03, 0.01)
mac(fx, age)
#> [1] 29.8
```
