# Leslie matrix summary

Leslie matrix summary

## Usage

``` r
# S3 method for class 'leslie'
summary(object, d = 1, ...)
```

## Arguments

- object:

  A leslie matrix from
  [`leslie()`](https://vadvu.github.io/demor/reference/leslie.md).

- d:

  Time step. By default it is 1. Only affects \\r\\.

- ...:

  Ignored.

## Value

A list of class `summary.leslie` with four components: `lambda`
(dominant eigenvalue / asymptotic growth factor), `r` (intrinsic growth
rate), `w` (stable age distribution summing to 1), and `v` (reproductive
values normalized so that `sum(v * w) = 1`).

## Details

The function calculates \\\lambda\\, \\r\\, \\w\\ and \\v\\.

- \\\lambda\\ – asymptotic growth factor that is dominant eigenvalue.

- \\r\\ – asymptotic growth rate that is \\ln \lambda / \delta t\\.

- \\w\\ – stable age distribution normalized to 1 s.t. \\\sum_x^{\omega}
  w_x = 1\\ where \\x\\ is age.

- \\v\\ – reproductive values normalized s.t. \\v'w = 1\\.

## See also

[`leslie()`](https://vadvu.github.io/demor/reference/leslie.md)

## Examples

``` r
mx <- c(0.02, 0.01, 0.012, 0.015, 0.02)
fx <- c(0.05, 0.08)
les <- leslie(mx = mx, fx = fx, age.mx = 0:4, age.fx = 1:2)
summary(les)
#> $lambda
#> [1] 0.9802469
#> 
#> $r
#> [1] -0.01995082
#> 
#> $w
#> [1] 0 0 0 0 1
#> 
#> $v
#> [1] 1.082128 1.071921 1.027973 1.000000 1.000000
#> 
#> attr(,"class")
#> [1] "summary.leslie"
```
