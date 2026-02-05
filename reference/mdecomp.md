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

Dataframe with 1st column as overall decomposition (`ex12`), and other
columns are decomposition by causes (`cause(i)`)

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
