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

(List of) Matrices with projected population. Columns represent periods,
rows represent age groups. The first column is `N0` (initial period).

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
