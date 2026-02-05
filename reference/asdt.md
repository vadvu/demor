# Associated single decrement life table (ASDT) for causes of death (cause-deleted life table)

Associated single decrement life table (ASDT) for causes of death
(cause-deleted life table)

## Usage

``` r
asdt(age, m_all, m_i, full = FALSE, method = "chiang1968", ...)
```

## Arguments

- age:

  Numeric array of age intervals; for full life table = `0:100`; for
  concise life table = `c(0:1, seq(5,85,5))`

- m_all:

  Numeric array with age specific mortality rates of all causes of death
  (usual mx).

- m_i:

  Numeric array with age specific mortality rates of some cause of death
  (i)

- full:

  Logical. Is full table needed? `TRUE` for full, `FALSE` for concise.
  By default, `FALSE`

- method:

  Character. The method of ASDT construction to use. Now just
  "chiang1968" is supported.

- ...:

  Optional. Additional arguments for
  [`LT()`](https://vadvu.github.io/demor/reference/LT.md) function.

## Value

dataframe.

## References

Chiang, L. (1968). *Introduction to Stochastic Processes in
Biostatistics*. New York: John Wiley and Sons.
