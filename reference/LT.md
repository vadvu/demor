# Life table

Life table

## Usage

``` r
LT(age, sex = "m", mx, ax = NULL, w = NULL, l0 = 1)
```

## Arguments

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- sex:

  Character. Sex. `"m"` for males or `"f"` for females. By default =
  `"m"`.

- mx:

  Numeric array with age specific mortality rates.

- ax:

  Optional. Numeric array with ax. By default, it is the middle of the
  interval, while ax for age 0 is modeled as in Andreev & Kingkade
  (2015).

- w:

  Optional. Numeric array with weights for each age interval for
  calculating weighted life expectancy (`wex`).

- l0:

  Numeric. Life table radix. By default, = `1` but it can be any
  positive real number. In "human" demography tradition it is 100'000,
  in "ecological" and "evolutionary" demography tradition it is 1.

## Value

A numeric matrix with one row per age group. Standard columns are `age`,
`mx`, `ax`, `qx`, `lx`, `dx`, `Lx`, `Tx`, and `ex`. If `w` is supplied,
additional columns `w`, `wLx`, and `wex` are appended, where `wex` is
weighted life expectancy.

## Details

By default, \\a_x\\ for age 0 (first entity in `ax`) is modeled as in
Andreev & Kingkade (2015, p. 390, see table 3-2).

The weighted life expectancy is calculated as follows: \$\$e_x^w =
\frac{\sum\_{i = x}^{\omega}L_x w_x}{l_x}\$\$ where \\\omega\\ is the
last age, \\w\\ is weight s.t. \\w \in \[0,1\]\\, and other variables
are life table functions.

## References

Andreev, E. M., & Kingkade, W. W. (2015). Average age at death in
infancy and infant mortality level: Reconsidering the Coale-Demeny
formulas at current levels of low mortality. *Demographic Research*,
*33*, 363-390.

## See also

[`MLT()`](https://vadvu.github.io/demor/reference/MLT.md) for Multiple
Decrement Life Table.

## Examples

``` r
# Minimal toy example
age <- 0:5
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
LT(age = age, sex = "m", mx = mx)
#>      age    mx     ax      qx      lx      dx       Lx       Tx    ex
#> [1,]   0 0.020  0.109 0.01965 1.00000 0.01965  0.99785 35.69312 35.69
#> [2,]   1 0.010  0.500 0.00995 0.98035 0.00975  0.97547 34.69527 35.39
#> [3,]   2 0.012  0.500 0.01193 0.97060 0.01158  0.96481 33.71979 34.74
#> [4,]   3 0.015  0.500 0.01489 0.95902 0.01428  0.95188 32.75499 34.15
#> [5,]   4 0.020  0.500 0.01980 0.94474 0.01871  0.93539 31.80311 33.66
#> [6,]   5 0.030 33.333 1.00000 0.92603 0.92603 30.86772 30.86772 33.33
# \donttest{
if (interactive()) {
  # Real RosBris data via get_rosbris(): Russian males, 2010
  rus2010 <- subset(get_rosbris("mortality_5"),
    year == 2010 & code == 1100 & sex == "m" & territory == "t"
  )
  LT(age = rus2010$age, sex = "m", mx = rus2010$mx)
}
# }
```
