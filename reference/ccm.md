# Cohort Component Model for Projecting Population.

Cohort Component Model for Projecting Population.

## Usage

``` r
ccm(
  Mx.f,
  Mx.m = NULL,
  Fx,
  Ix.f = NULL,
  Ix.m = NULL,
  age.mx,
  age.fx,
  N0.f,
  N0.m = NULL,
  srb = 100/205,
  ...
)
```

## Arguments

- Mx.f:

  Matrix or dataframe with mx for females, where row represents age,
  column represents a forecasted period. Thus, first column should be
  the first projected period, while each row represents age-specific
  mortality rate.

- Mx.m:

  Optional. Matrix or dataframe with mx for males, where row represents
  age, column represents a forecasted period. Thus, first column should
  be the first projected period, while each row represents age-specific
  mortality rate.

- Fx:

  Matrix or dataframe with fx, where row represents age, column
  represents a forecasted period. Thus, first column should be the first
  projected period, while each row represents age-specific fertility
  rate.

- Ix.f:

  Optional. Matrix or dataframe with net number of female migrants,
  where row represents age, column represents a forecasted period. Thus,
  first column should be the first projected period, while each row
  represents net number of migrants.

- Ix.m:

  Optional. Matrix or dataframe with net number of male migrants, where
  row represents age, column represents a forecasted period. Thus, first
  column should be the first projected period, while each row represents
  net number of migrants.

- age.mx:

  Numeric vector. Age groups for Mx.f rows.

- age.fx:

  Numeric vector. Age groups for Fx rows.

- N0.f:

  Numeric vector. Female population in the initial period by age groups.

- N0.m:

  Optional. Numeric vector. Male population in the initial period by age
  groups.

- srb:

  Numeric. Sex ratio at birth for females. By default, it is 100/205.

- ...:

  Optional. Additional arguments for
  [`leslie()`](https://vadvu.github.io/demor/reference/leslie.md)
  function.

## Value

If `Mx.m` is `NULL`, a numeric matrix with projected female population,
where rows are age groups and columns are periods `0:h`. If male
mortality is supplied, a list with matrices `female`, `male`, and `all`,
each with the same row/column structure.

## Details

The model is calculated in matrix form as \$\$\mathbf{N}\_{t+h} =
\mathbf{L}\_t(\mathbf{N}\_{t} + \mathbf{I}\_{t}/2) +
\mathbf{I}\_{t}/2\$\$ where \\\mathbf{N}\_t\\ is a column vector of
population for time \\t\\ with \\h\\ as a step of projection (it is the
length of age interval), \\\mathbf{L}\_t\\ is Leslie matrix and
\\\mathbf{I}\_{t}\\ is a column vector of net migration.

Note that the model assumes that in \\\mathbf{N}\\ all age intervals are
the same (i.e. \\age = \\0-1, 1-4, 5-9, ...\\\\ is not permitted).
Fortunately (and thanks to me), the function handles such situations
automatically by transforming all age groups into a unified standard.

## Examples

``` r
age.mx <- seq(0, 80, 5)
age.fx <- seq(15, 45, 5)
Mx.f <- matrix(
  rep(seq(0.005, 0.12, length.out = length(age.mx)), 2),
  nrow = length(age.mx),
  ncol = 2
)
Fx <- matrix(
  rep(c(0.02, 0.08, 0.11, 0.09, 0.05, 0.02, 0.005), 2),
  nrow = length(age.fx),
  ncol = 2
)
N0.f <- round(100000 * exp(-0.04 * age.mx))
ccm(Mx.f = Mx.f, Fx = Fx, age.mx = age.mx, age.fx = age.fx, N0.f = N0.f)
#>         0         1
#> 0  100000 32304.435
#> 5   81873 94739.033
#> 10  67032 75711.087
#> 15  54881 59813.167
#> 20  44933 47249.885
#> 25  36788 37322.133
#> 30  30119 29476.495
#> 35  24660 23276.574
#> 40  20190 18378.378
#> 45  16530 14507.862
#> 50  13534 11449.598
#> 55  11080  9034.409
#> 60   9072  7125.721
#> 65   7427  5619.607
#> 70   6081  4429.560
#> 75   4979  3490.480
#> 80   4076  4932.932
```
