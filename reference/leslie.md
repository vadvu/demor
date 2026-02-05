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

Matrix.

## See also

[`summary.leslie()`](https://vadvu.github.io/demor/reference/summary.leslie.md)
for `leslie` output that calculates \\\lambda\\, \\r\\, \\w\\ and \\v\\.
