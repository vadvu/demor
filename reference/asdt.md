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

If `full = TRUE`, a data frame with the full associated single decrement
life table, including the standard life-table columns and the
cause-deleted columns `r_not_i`, `p_not_i`, `l_not_i`, `a_not_i`,
`d_not_i`, `L_not_i`, `T_not_i`, and `ex_without_i`. If `full = FALSE`,
a reduced data frame with columns `age`, `r_not_i`, `lx`, `qx`, `ax`,
`ex`, `p_not_i`, `l_not_i`, `a_not_i`, and `ex_without_i`.

## References

Chiang, L. (1968). *Introduction to Stochastic Processes in
Biostatistics*. New York: John Wiley and Sons.

## Examples

``` r
data(asdtex)
asdt(
  age = asdtex$age,
  m_all = asdtex$all,
  m_i = asdtex$circulatory
)[1:3, ]
#>   age   r_not_i      lx      qx    ax    ex   p_not_i   l_not_i   a_not_i
#> 1   0 0.9772256 1.00000 0.00757 0.134 74.65 0.9926018 1.0000000 0.1340748
#> 2   1 0.9561720 0.99243 0.00141 2.000 74.22 0.9986518 0.9926018 2.0000618
#> 3   5 0.9665151 0.99103 0.00084 2.500 70.32 0.9991881 0.9912635 2.5000352
#>   ex_without_i
#> 1        86.59
#> 2        86.23
#> 3        82.34
```
