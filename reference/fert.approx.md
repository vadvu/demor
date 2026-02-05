# Fertility models for ASFR approximation

Fertility models for ASFR approximation

## Usage

``` r
fert.approx(fx, age, model, start = NULL, se = FALSE, alpha = 0.05, bn = 1000)
```

## Arguments

- fx:

  Numeric vector of age specific fertility rates.

- age:

  Numeric vector of ages.

- model:

  Character. Model name to be estimated. Now "Hadwiger", "Gamma",
  "Brass" and "Beta" are supported.

- start:

  Numeric vector with user-specific values of parameters for
  optimization. Default is `NULL` (choose automatically)

- se:

  Logical. Should bootstrapped variance for ASFR approximation be
  calculated. Default is `FALSE` for no bootstrap.

- alpha:

  Numeric. Used if `se = TRUE`, the level of uncertainty. By default,
  `alpha = 0.05` for 95% CI.

- bn:

  Numeric. Used if `se = TRUE`, number of bootstrap samples. By default,
  `bn = 1000`.

## Value

list with estimated model (parameters, variance-covariance matrix,
percentiles of parameters) and dataframe with predicted and observed
ASFR as well as SE and percentile of predictions

## Details

This function runs least squares optimization (using default `optim`) of
the selected fertility function with 1e-06 as tolerance parameter.

\\f_x\\ is age-specific fertility rate for age \\x\\.

### Hadwiger model

The model is as follows: \$\$f_x = \frac{ab}{c} \frac{c}{x}^{3/2}
exp\[-b^2(\frac{c}{x}+\frac{x}{c}-2)\]\$\$ where \\a,b,c\\ are estimated
parameters that do not have demographic interpretation. Sometimes \\c\\
is interpreted as mean age at childbearing.

### Gamma model

The model is as follows: \$\$f_x = \frac{R}{\Gamma(b)c^b}(x-d)^{b-1}
exp\[-(\frac{x-d}{c})\]\$\$ where \\R,b,c,d\\ are estimated parameters.
\\\Gamma\\ is gamma function. \\R\\ can be interpreted as fertility
level (TFR) and \\d\\ as mean age at childbearing.

### Brass model

The model is as follows: \$\$f_x = c(x-d)(d+w-x)\$\$ where \\c,d,w\\ are
estimated parameters.

### Beta model

The model is as follows: \$\$f_x = \frac{R}{\Beta(A,C)}(\beta -
\alpha)^{-(A+C-1)}(x-\alpha)^{(A-1)}(\beta-x)^{(B-1)}\$\$ where
\\\Beta\\ is beta function, \\R, \beta, \alpha\\ are estimated
parameters, which can be interpreted as fertility level (TFR) and max
and min age of childbearing respectively. \\A,C\\ are \$\$C =
(\frac{(v - \alpha)(\beta - v)}{\tau^2} - 1)\frac{\beta - v}{\beta -
\alpha}\$\$ \$\$A = C\frac{v-\alpha}{v - \beta}\$\$ where \\v, \tau^2\\
are estimated parameters, where \\v\\ can be interpreted as mean age at
childbearing. Thus, Beta model uses 5 parameters \\R, \beta, \alpha, v,
\tau^2\\, where only \\\tau^2\\ has no demographic interpretation.

## References

Peristera, P., & Kostaki, A. (2007). Modeling fertility in modern
populations. *Demographic Research*, *16*, 141-194.

## Examples

``` r
# fert.approx(fx = ASFR, age = 15:55, model = "Hadwiger", se = FALSE)
```
