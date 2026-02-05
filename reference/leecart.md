# Lee-Carter model

Lee-Carter model

## Usage

``` r
leecart(
  data,
  n = 10,
  alpha = 0.05,
  model = "RWwD",
  ax_method = "classic",
  bx_method = "classic",
  boot = FALSE,
  bn = 1000,
  ktadj = "none",
  ...
)
```

## Arguments

- data:

  Dataframe in the long format with the following columns: `age`,
  `year`, `mx` (age specific mortality rates). For some types of `ktadj`
  argument `N` (population at age x) and `Dx` (number of deaths at
  age x) columns should also be presented.

- n:

  Numeric. Forecasted horizon

- alpha:

  Numeric. The level of uncertainty. By default, `alpha = 0.05` for 95%
  CI.

- model:

  Character. Model type for kt forecasting. Can be "RWwD" for random
  walk with drift (by default, for original Lee-Carter model) or "ARIMA"
  for ARIMA model which parameters are chosen automatically by
  [`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html).

- ax_method:

  Character. Method for ax calculation. Can be "classic" from original
  Lee-Carter model (by default), "last" or "last_smooth". See details.

- bx_method:

  Character. Method for bx calculation. Can be "classic" from original
  Lee-Carter model (by default) and "rotate" for rotating bx (Li et al.,
  2013).

- boot:

  Logical. Should bootstrap estimates for uncertainty be used? `FALSE`
  by default.

- bn:

  Numeric. Used if `boot = TRUE`, number of bootstrap samples. By
  default, `bn = 1000`.

- ktadj:

  Character. Type of `kt` adjustment. It can be set to 'none' (defaukt,
  no adjustment), 'Dmin', 'e0min', 'poisson' or 'edaggermin' (see
  Details). Note that 'Dmin' and 'poisson' require data on the
  age-specific number of deaths (`Dx` column in the data) and the
  age-specific population (`N` column in the data).

- ...:

  Optional. Additional arguments for
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) function.

## Value

Dataframe with the projected mx and ex for t+n periods with mean, low95
and high 95 values

## Details

The **`model`** argument specifies the forecasting method.

- `model ="RWwD"` – classic random walk option

- `model = "ARIMA"` for selecting a more complex time series model

The **`ax_method`** argument allows to control how `a_x` is calculated.

- `ax_method = "classic"` – classic option with the average of the
  logarithm of mortality rates (but there is so-called "jump-off bias").

- `ax_method = "last"` uses the logarithm of mortality for the last
  available year (as proposed in Lee & Miller, 2001).

- `ax_method = "last_smooth"` uses data for the last year with smoothing
  (see Ševčíková et al., 2016, p. 288).

The **`bx_method`** argument allows to control how `b_x` is calculated.

- `bx_method = "classic"` for the original method.

- `bx_method = "rotate"` for the rotational variant (see Li et al.,
  2013).

The **`ktadj`** argument allows to control how `k_t` is calculated.

- `ktadj = "none"` for no adjustment.

- `ktadj = "Dmin"` for minimizing the deviance of predicted/actual
  annual deaths (as proposed in the original Lee-Carter paper). This
  method requires data on the age-specific number of deaths (`Dx` column
  in the data) and the age-specific population (`N` column in the data).

- `ktadj = "e0min"` for minimizing the deviance of predicted/actual life
  expectancy (as proposed in Lee & Miller, 2001).

- `ktadj = "poisson"` for minimizing the deviance from a Poisson model,
  where the dependent variable is the age-specific annual number of
  deaths (as proposed in Booth et al., 2002). This method requires data
  on the age-specific number of deaths (`Dx` column in the data) and the
  age-specific population (`N` column in the data).

- `ktadj = "edaggermin"` for minimizing the deviance of predicted/actual
  edagger (see
  [`edagger()`](https://vadvu.github.io/demor/reference/edagger.md)) as
  proposed in Rabbi & Mazzuco, 2021.

## References

Booth, H., Maindonald, J., & Smith, L. (2002). Applying Lee-Carter under
conditions of variable mortality decline. Population studies, 56(3),
325-336. <https://doi.org/10.1080/00324720215935>

Lee, R. D., & Carter, L. R. (1992). Modeling and forecasting US
mortality. Journal of the American Statistical Association, 87(419),
659–671. <https://doi.org/10.1080/01621459.1992.10475265>

Lee, R., & Miller, T. (2001). Evaluating the performance of the
lee-carter method for forecasting mortality. Demography, 38(4), 537–549.
<https://doi.org/10.1353/dem.2001.0036>

Li, N., Lee, R., & Gerland, P. (2013). Extending the Lee-Carter Method
to Model the Rotation of Age Patterns of Mortality Decline for Long-Term
Projections. Demography, 50(6), 2037–2051.
<https://doi.org/10.1007/s13524-013-0232-2>

Rabbi, A. M. F., & Mazzuco, S. (2021). Mortality forecasting with the
lee–carter method: Adjusting for smoothing and lifespan disparity.
European Journal of Population, 37(1), 97-120.
<https://doi.org/10.1007/s10680-020-09559-9>

Ševčíková, H., Li, N., Kantorová, V., Gerland, P., & Raftery, A. E.
(2016). Age-Specific Mortality and Fertility Rates for Probabilistic
Population Projections. In R. Schoen (Ed.), Dynamic Demographic Analysis
(Vol. 39, pp. 285–310). Springer International Publishing.
<https://doi.org/10.1007/978-3-319-26603-9_15>
