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

Extended `LT` matrix

## See also

[`LT()`](https://vadvu.github.io/demor/reference/LT.md) for usual life
table calculation
