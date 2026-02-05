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

Matrix of (age x 9). Columns are: age, mx, ax, qx, lx, dx, Lx, Tx, ex

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
# Real data from demor: Russian males, 2010
rus2010 <- subset(rosbris_mortality_pop_5,
  year == 2010 & code == 1100 & sex == "m" & territory == "t"
)
LT(age = rus2010$age, sex = "m", mx = rus2010$mx)
#>       age      mx    ax      qx      lx      dx      Lx       Tx    ex
#>  [1,]   0 0.00882 0.132 0.00876 1.00000 0.00876 0.99885 63.04122 63.04
#>  [2,]   1 0.00060 2.000 0.00241 0.99124 0.00239 3.96019 62.04238 62.59
#>  [3,]   5 0.00036 2.500 0.00177 0.98885 0.00175 4.93988 58.08218 58.74
#>  [4,]  10 0.00039 2.500 0.00193 0.98710 0.00191 4.93072 53.14231 53.84
#>  [5,]  15 0.00119 2.500 0.00591 0.98519 0.00582 4.91139 48.21159 48.94
#>  [6,]  20 0.00255 2.500 0.01265 0.97937 0.01239 4.86586 43.30020 44.21
#>  [7,]  25 0.00449 2.500 0.02221 0.96698 0.02147 4.78120 38.43434 39.75
#>  [8,]  30 0.00681 2.500 0.03347 0.94550 0.03165 4.64841 33.65314 35.59
#>  [9,]  35 0.00793 2.500 0.03890 0.91386 0.03555 4.48042 29.00473 31.74
#> [10,]  40 0.00978 2.500 0.04774 0.87831 0.04193 4.28672 24.52431 27.92
#> [11,]  45 0.01335 2.500 0.06461 0.83638 0.05404 4.04679 20.23759 24.20
#> [12,]  50 0.01857 2.500 0.08872 0.78234 0.06941 3.73817 16.19080 20.70
#> [13,]  55 0.02625 2.500 0.12317 0.71293 0.08781 3.34513 12.45263 17.47
#> [14,]  60 0.03714 2.500 0.16994 0.62512 0.10623 2.86003  9.10751 14.57
#> [15,]  65 0.04992 2.500 0.22193 0.51889 0.11516 2.30657  6.24748 12.04
#> [16,]  70 0.06860 2.500 0.29278 0.40374 0.11821 1.72316  3.94091  9.76
#> [17,]  75 0.09764 2.500 0.39240 0.28553 0.11204 1.14754  2.21775  7.77
#> [18,]  80 0.13804 2.500 0.51313 0.17349 0.08902 0.64489  1.07021  6.17
#> [19,]  85 0.19859 5.035 1.00000 0.08447 0.08447 0.42532  0.42532  5.04
# }
```
