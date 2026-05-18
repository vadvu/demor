# Multiple Decrement Life Table

Multiple Decrement Life Table

## Usage

``` r
MLT(age, mx, ...)
```

## Arguments

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- mx:

  List of numeric arrays. 1st array should be all-cause mx in the
  population, other arrays are cause-specific mx in the population.

- ...:

  Other parameters for the function
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) including `ax`
  (by default, the middle of the interval), `sex` (by default = `"m"` -
  males), `l0` (by default = 1).

## Value

A numeric matrix extending the output of
[`LT()`](https://vadvu.github.io/demor/reference/LT.md). In addition to
the standard life-table columns, for each cause `i` in `mx[-1]` it adds
`qx_i`, `dx_i`, `lx_i`, and `ex_no_i`, corresponding to cause-specific
death probabilities, deaths, survivors, and cause-deleted life
expectancy.

## See also

[`LT()`](https://vadvu.github.io/demor/reference/LT.md) for usual life
table calculation

## Examples

``` r
data(asdtex)
mx_causes <- list(
  all = asdtex$all,
  neoplasms = asdtex$neoplasms,
  circulatory = asdtex$circulatory
)
MLT(age = asdtex$age, mx = mx_causes)[1:3, c("age", "mx", "qx_neoplasms", "ex_no_neoplasms")]
#>      age      mx qx_neoplasms ex_no_neoplasms
#> [1,]   0 0.00762      0.00003           84.29
#> [2,]   1 0.00035      0.00012           83.93
#> [3,]   5 0.00017      0.00015           80.03
```
