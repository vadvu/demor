# Leslie Matrix

Leslie Matrix

## Usage

``` r
leslie(mx, fx, age.mx, age.fx, srb = 100/205, fin = TRUE, ...)
```

## Arguments

- mx:

  Numeric array of age specific mortality rates.

- fx:

  Numeric array of age specific fertility rates.

- age.mx:

  Numeric array of ages for mx.

- age.fx:

  Numeric array of ages for fx.

- srb:

  Numeric. Sex ratio at birth. Usually it is assumed that for males it
  is `105/205`, for females it is `100/205`. By default, it is
  `100/205`.

- fin:

  Logical. Should the survival rate for the last age-group be nonzero?
  By default it is `FALSE`, so the last survival rate is 0 as in
  classical model. Otherwise, it is \\T\_{x}/T\_{x-1}\\.

- ...:

  Optional. Additional arguments for
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) function.

## Value

A square numeric matrix of class `leslie` with one row and one column
per age group. The first row contains fertility contributions and the
subdiagonal contains survival ratios.

## See also

[`summary.leslie()`](https://vadvu.github.io/demor/reference/summary.leslie.md)
for `leslie` output that calculates \\\lambda\\, \\r\\, \\w\\ and \\v\\.

## Examples

``` r
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02)
fx <- c(0.05, 0.08)
leslie(mx = mx, fx = fx, age.mx = 0:4, age.fx = 1:2)
#>            [,1]       [,2]       [,3]      [,4]      [,5]
#> [1,] 0.01189598 0.03142637 0.01947024 0.0000000 0.0000000
#> [2,] 0.97757178 0.00000000 0.00000000 0.0000000 0.0000000
#> [3,] 0.00000000 0.98907193 0.00000000 0.0000000 0.0000000
#> [4,] 0.00000000 0.00000000 0.98659840 0.0000000 0.0000000
#> [5,] 0.00000000 0.00000000 0.00000000 0.9802469 0.9802469
#> attr(,"class")
#> [1] "leslie"
```
