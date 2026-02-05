# Tempo-adjusted total fertility rate (TFR')

Tempo-adjusted total fertility rate (TFR')

## Usage

``` r
tatfr(past_fx, present_fx, post_fx, age)
```

## Arguments

- past_fx:

  List with numeric arrays of age specific fertility rates for period
  t-1 by parity

- present_fx:

  List with numeric arrays of age specific fertility rates for period t
  by parity (it is period of interest)

- post_fx:

  List with numeric arrays of age specific fertility rates for period
  t+1 by parity

- age:

  Array with numeric values age

## Value

list with TFR' (tatfr) and TFR' by parity (tatfr_i, in user-specific
order as in lists), TFR (tfr) and TFR by parity (tfr_i)

## Details

This indicator is calculated as follows \$\$TFR\_{i,t}' =
\frac{TFR\_{i,t}}{1-(M\_{i,t+1} - M\_{i,t-1}) / 2}\$\$ where
\\TFR\_{i,t}', TFR\_{i,t}\\ are tempo-adjusted and usual total fertility
rate for parity \\i\\ and time \\t\\ respectively, \\M\_{i,t}\\ is mean
age at childbearing for parity \\i\\ and time \\t\\. The tempo-adjusted
total fertility rate is a sum of parity-specific \\TFR_i'\\.

Note, the calculation are done as in footnote 1 in (Bongaarts & Feeney,
2000, p. 563). Unfortunately, the original 1998 article does not provide
the exact formula, which has caused some confusion in academic circles.

## References

Bongaarts, J., & Feeney, G. (1998). On the Quantum and Tempo of
Fertility. Population and Development Review, 24(2), 271–291.
https://doi.org/10.2307/2807974

Bongaarts, J., & Feeney, G. (2000). On the Quantum and Tempo of
Fertility: Reply. Population and Development Review, 26(3), 560–564.
https://doi.org/10.1111/j.1728-4457.2000.00560.x

## See also

[`tfr()`](https://vadvu.github.io/demor/reference/tfr.md) for TFR and
[`mac()`](https://vadvu.github.io/demor/reference/mac.md) for mean age
at childbearing calculation.
