# Mortality models for mx approximation

Mortality models for mx approximation

## Usage

``` r
mort.approx(mx, age, model = c("Brass", "Gompertz"), standard.mx = NULL, ...)
```

## Arguments

- mx:

  Numeric vector of age specific mortality rates.

- age:

  Numeric vector of ages.

- model:

  Character. Model name to be estimated. Now "Gompertz" and "Brass" are
  supported.

- standard.mx:

  Numeric vector of age specific mortality rates for standard
  population. Default is `NULL`.

- ...:

  Used only for `Brass` model. Other parameters for the function
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) including `ax`
  (by default, the middle of the interval), `sex` (by default = `"m"` -
  males), `l0` (by default = 1).

## Value

list with estimated model and dataframe with predicted mortality rates.

## Details

This function runs least squares optimization of the selected mortality
function using Gauss-Newton algorithm algorithm with 2000 maximum
iterations and 1e-07 as tolerance parameter. For "Gompertz" usual OLS
estimator is used.

### Gompertz model

The model is as follows: \$\$m(age) = \alpha e^{\beta age}\$\$

### Brass model

The model is as follows: \$\$y(age) = \alpha + \beta y^{S}(age)\$\$
where \$\$y(age) = \frac{1}{2} ln\[\frac{q(age)}{1-q(age)}\]\$\$ and
subscript S defines that function is for standard population. To get mx
from qx usual formula is used: \$\$m(age) =
\frac{q(age)}{n-q(age)(n-a(age))}\$\$ where n is the size of age
interval and a(x) is a parameter from life table.

## References

Preston, S. H., Heuveline, P., & Guillot, M. (2001). Demography:
Measuring and modeling population processes. Blackwell Publishers.
([pdf](https://gwern.net/doc/statistics/2001-preston-demography.pdf))

## Examples

``` r
# mort.approx(mx = mx, age = 0:100, model = "Brass", standard.mx = standard.mx, sex = "m")
```
