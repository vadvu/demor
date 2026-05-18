# Split continuous ages to age groups

Split continuous ages to age groups

## Usage

``` r
ages(x, groups, char = FALSE, below_min_val = NA)
```

## Arguments

- x:

  Numeric array. Values of age that should be grouped

- groups:

  Numeric array. Values of lower boundaries of age groups

- char:

  Logical. Should output be numeric (`FALSE`) or character (`TRUE`)? By
  default, `FALSE`

- below_min_val:

  What value to return for x \< min(groups) when char = `FALSE`? Default
  is NA.

## Value

If `char = TRUE`, a factor of the same length as `x` with age-group
labels. Otherwise, a numeric vector of the same length as `x` with the
lower boundaries from `groups`; values below the first group receive
`below_min_val`.

## Examples

``` r
groups <- c(0, 1, 5, 10, 15, 20, 24, 45, 85)
x <- 0:100
ages(x, groups, TRUE)
#>   [1] 0-1   1-4   1-4   1-4   1-4   5-9   5-9   5-9   5-9   5-9   10-14 10-14
#>  [13] 10-14 10-14 10-14 15-19 15-19 15-19 15-19 15-19 20-23 20-23 20-23 20-23
#>  [25] 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44
#>  [37] 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 24-44 45-84 45-84 45-84
#>  [49] 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84
#>  [61] 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84
#>  [73] 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84 45-84
#>  [85] 45-84 85+   85+   85+   85+   85+   85+   85+   85+   85+   85+   85+  
#>  [97] 85+   85+   85+   85+   85+  
#> Levels: <0 0-1 1-4 5-9 10-14 15-19 20-23 24-44 45-84 85+
```
