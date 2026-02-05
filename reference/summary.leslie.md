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

## Details

The function calculates \\\lambda\\, \\r\\, \\w\\ and \\v\\.

- \\\lambda\\ – asymptotic growth factor that is dominant eigenvalue.

- \\r\\ – asymptotic growth rate that is \\ln \lambda / \delta t\\.

- \\w\\ – stable age distribution normalized to 1 s.t. \\\sum_x^{\omega}
  w_x = 1\\ where \\x\\ is age.

- \\v\\ – reproductive values normalized s.t. \\v'w = 1\\.

## See also

[`leslie()`](https://vadvu.github.io/demor/reference/leslie.md)
