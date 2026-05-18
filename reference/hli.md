# The Human Life Indicator (HLI)

The Human Life Indicator (HLI)

## Usage

``` r
hli(age, mx, ...)
```

## Arguments

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- mx:

  Numeric array with age specific mortality rates.

- ...:

  Optional. Additional arguments for
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) function.

## Value

A length-1 numeric value giving the Human Life Indicator, i.e. the
geometric mean age at death implied by the life table.

## Details

It is calculated as \$\$HLI = \prod\_{x=\alpha}^{\omega}(x +
a_x)^{d_x}\$\$ where \\\alpha, \omega\\ are the first and last age
groups, \\x\\ is age, \\a_x, d_x\\ are life table functions (s.t.
\\\sum\_{x=\alpha}^{\omega} d_x = 1\\).

## References

Ghislandi, S., Sanderson, W.C., & Scherbov, S. (2019). A Simple Measure
of Human Development: The Human Life Indicator. *Population and
Development Review*, *45*, 219–233.

## Examples

``` r
age <- 0:5
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
hli(age, mx)
#> [1] 29.77401
```
