# Download RosBris Data

Download age-specific mortality, fertility, and population data from the
Russian Fertility and Mortality Database (RosBris) and return them as
long-format data frames for use in `demor`.

## Usage

``` r
get_rosbris(
  dataset = c("mortality_1", "mortality_5", "fertility_1", "fertility_5"),
  refresh = FALSE
)
```

## Source

Russian Fertility and Mortality Database. Center for Demographic
Research, Moscow (Russia). Available at
<https://www.nes.ru/research-main/research-centers/demogr/demogr-fermort-data>

## Arguments

- dataset:

  Character. Dataset to download and parse. One of `"mortality_1"`,
  `"mortality_5"`, `"fertility_1"`, or `"fertility_5"`.

- refresh:

  Logical. If `FALSE` (default), previously downloaded RosBris archives
  are reused from the local cache directory. If `TRUE`, archives are
  downloaded again from the RosBris website.

## Value

A data frame. Column structure depends on `dataset`; see **Details**.

## Details

The function downloads official `.zip` archives from the RosBris
website, stores them in a user cache directory created by
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), reads
`.txt` tables from the archives, and converts them to long-format data
frames.

Returned data have the following structure:

- `"mortality_1"`: `year`, `code`, `territory`, `sex`, `age`, `mx`, `N`,
  `Dx`, `name`.

- `"mortality_5"`: `year`, `code`, `territory`, `sex`, `age`, `mx`, `N`,
  `Dx`, `name`.

- `"fertility_1"`: `year`, `code`, `territory`, `age`, `fx`, `N`, `Bx`,
  `name`.

- `"fertility_5"`: `year`, `code`, `territory`, `age`, `fx`, `fx1`,
  `fx2`, `fx3`, `fx4`, `fx5`, `N`, `Bx`, `Bx1`, `Bx2`, `Bx3`, `Bx4`,
  `Bx5`, `name`.

At the moment, `get_rosbris()` uses the legacy RosBris series
corresponding to the periods `1989-2014` and `2015-2022`. Updated
post-census series are not used by this function.

## See also

[rosbris.codes](https://vadvu.github.io/demor/reference/rosbris.codes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mort <- get_rosbris("mortality_5")
fert <- get_rosbris("fertility_1")

rus2010 <- subset(
  mort,
  year == 2010 & code == 1100 & sex == "m" & territory == "t"
)
} # }
```
