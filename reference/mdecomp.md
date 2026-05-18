# Age and cause decomposition of differences in life expectancies

Age and cause decomposition of differences in life expectancies

## Usage

``` r
mdecomp(mx1, mx2, age, method = "andreev", ...)
```

## Arguments

- mx1:

  List of numeric arrays. 1st array should be all-cause nmx in the 1st
  population, other arrays are cause-specific nmx in the 1st population

- mx2:

  List of numeric arrays. 1st array should be all-cause nmx in the 2nd
  population, other arrays are cause-specific nmx in the 2nd population

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- method:

  Character. Decomposition method. "andreev" (1982) or
  "arriaga" (1984) - slightly different in their results. By default,
  `method = "andreev"`.

- ...:

  Optional. Additional arguments for
  [`decomp()`](https://vadvu.github.io/demor/reference/decomp.md).

## Value

A data frame of class `c("mdecomp", "data.frame")` with one row per age
group. Column `age` contains ages, `ex12` contains the overall age
contribution to the life-expectancy difference, and the remaining
columns contain cause-specific contributions named after the
cause-specific elements of `mx1` and `mx2`.

## Details

The contribution of each cause \\c\\ to the absolute difference in life
expectancies between the first and second population is caculated as
\$\$\Delta\_{x,c} = \frac{m^1\_{x,c} - m^2\_{x,c}}{m^1\_{x} - m^2\_{x}}
\times \Delta\_{x}\$\$ where \\\Delta\_{x}\\ is contribution of age
\\x\\ to difference \\e_0^2 - e_0^1\\ from function
[`decomp()`](https://vadvu.github.io/demor/reference/decomp.md),
\\m^i\_{x,c}\\ is age-specific mortality rate for population \\i\\ from
cause \\c\\, and \\m^i_x\\ is total age-specific mortality rate.

## See also

[`decomp()`](https://vadvu.github.io/demor/reference/decomp.md) for just
age decomposition and
[`plot.mdecomp()`](https://vadvu.github.io/demor/reference/plot.mdecomp.md)
for graph of `mdecomp` results

## Examples

``` r
data(mdecompex)
usa <- subset(mdecompex, cnt == "usa")
eng <- subset(mdecompex, cnt == "eng")
dec <- mdecomp(
  mx1 = list(
    all = usa$all,
    neoplasms = usa$neoplasms,
    circulatory = usa$circulatory
  ),
  mx2 = list(
    all = eng$all,
    neoplasms = eng$neoplasms,
    circulatory = eng$circulatory
  ),
  age = usa$age
)
dec[1:3, ]
#>   age ex12     neoplasms  circulatory
#> 1   0 0.13  0.0009474709 7.090137e-03
#> 2   1 0.03 -0.0025030458 5.483996e-04
#> 3   5 0.02 -0.0016468368 8.262058e-05
```
