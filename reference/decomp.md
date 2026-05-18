# Age decomposition of mortality: single decrement process

Age decomposition of mortality: single decrement process

## Usage

``` r
decomp(mx1, mx2, sex = "m", age, method = "andreev", ax1 = NULL, ax2 = NULL)
```

## Arguments

- mx1:

  Numeric array with age specific mortality rates of population 1 (base
  population).

- mx2:

  Numeric array with age specific mortality rates of population 2
  (compared population).

- sex:

  Character. Sex. "m" for males and "f" for females. By default,
  `sex = "m"`.

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- method:

  Character. Decomposition method. "andreev" (1982), "arriaga" (1984) or
  "pollard" (1982) - slightly different in their results. By default,
  `method = "andreev"`.

- ax1:

  Optional. Numeric array with ax for the 1st population. By default, it
  is a the middle of the interval, while ax for age 0 is modeled as in
  Andreev & Kingkade (2015).

- ax2:

  Optional. Numeric array with ax for the 2nd population. By default, it
  is a the middle of the interval, while ax for age 0 is modeled as in
  Andreev & Kingkade (2015).

## Value

A data frame with one row per age group. It always contains `age`,
`ex12`, and `ex12_prc`, where `ex12` is the absolute age contribution to
the life-expectancy difference and `ex12_prc` is the percentage
contribution. Additional columns depend on `method` and contain
intermediate life-table quantities used in the decomposition.

## Details

Example of decomposition using Andreev (1982) formulas: \$\$\Delta_x =
l_x^2(e_x^2 - e_x^1) - l\_{x+n}^2(e\_{x+n}^2 - e\_{x+n}^1), \\ \\ \\ x
\neq \omega\$\$ \$\$\Delta\_{\omega} = l\_{\omega}^2(e\_{\omega}^2 -
e\_{\omega}^1)\$\$ where \\\Delta_x\\ is an absolute contribution of age
\\x\\ to difference in \\e_0\\ between the second and the first
population. \\e_x^i, l_x^i\\ are life table functions for population
\\i\\. \\\omega\\ is the last age group. Note, \\e_0^2 - e_0^1 =
\sum\_{x}^{\omega}\Delta_x\\

## References

1.  Arriaga, E. E. (1984). Measuring and explaining the change in life
    expectancies. *Demography*, *21*, 83-96.

2.  Andreev, E. M. (1982). Metod komponent v analize prodolzhitel'nosti
    zhizni. *Vestnik statistiki*, *9*, 42-47.

3.  Pollard, J. H. (1982). The expectation of life and its relationship
    to mortality. *Journal of the Institute of Actuaries*, *109(2)*,
    225–240.

## See also

[`mdecomp()`](https://vadvu.github.io/demor/reference/mdecomp.md) for
age and cause decomposition

## Examples

``` r
age <- 0:5
mx1 <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
mx2 <- c(0.018, 0.009, 0.011, 0.014, 0.019, 0.028)
decomp(mx1, mx2, age = age)$ex12
#> [1] 0.08 0.03 0.04 0.03 0.04 2.21
```
