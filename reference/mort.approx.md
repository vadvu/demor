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

A list with two components: `model`, the fitted `lm` or `nls` object;
and `predicted`, a data frame with columns `age` and `mx.pred`
containing the fitted mortality schedule.

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
Measuring and Modeling Population Processes. Blackwell Publishers.

## Examples

``` r
age <- seq(40, 80, 10)
mx <- c(0.003, 0.005, 0.009, 0.018, 0.036)
standard.mx <- c(0.0025, 0.004, 0.007, 0.014, 0.03)
mort.approx(mx = mx, age = age, model = "Gompertz")
#> $model
#> 
#> Call:
#> stats::lm(formula = log(mx) ~ age)
#> 
#> Coefficients:
#> (Intercept)          age  
#>    -8.38237      0.06251  
#> 
#> 
#> $predicted
#>   age     mx.pred
#> 1  40 0.002789002
#> 2  50 0.005210930
#> 3  60 0.009736027
#> 4  70 0.018190652
#> 5  80 0.033987150
#> 
mort.approx(mx = mx, age = age, model = "Brass", standard.mx = standard.mx)
#> $model
#> Nonlinear regression model
#>   model: 0.5 * log((qx/(1 - qx))) ~ a + b * stand.logit
#>    data: parent.frame()
#>     a     b 
#> 0.189 1.050 
#>  residual sum-of-squares: 0.0001375
#> 
#> Number of iterations to convergence: 1 
#> Achieved convergence tolerance: 2.517e-08
#> 
#> $predicted
#>   age     mx.pred
#> 1  40 0.003037349
#> 2  50 0.004955936
#> 3  60 0.008879215
#> 4  70 0.018178505
#> 5  80 0.030000300
#> 
```
