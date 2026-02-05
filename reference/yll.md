# Years of Life Lost (YLL) calculation

Years of Life Lost (YLL) calculation

## Usage

``` r
yll(
  Dx,
  type = c("yll", "yll.p", "yll.r", "asyr"),
  age.int = 5,
  Dx_all = NULL,
  pop = NULL,
  w = NULL,
  standard = NULL
)
```

## Arguments

- Dx:

  Array with the number of deaths.

- type:

  Character. Type of YLL to calculate. See details section.

- age.int:

  Numeric. Age interval of Dx. Can be `1` or `5`.

- Dx_all:

  Array with the number of all deaths. Used only with `yll.p` type,
  where `Dx` is array with cause-specific deaths.

- pop:

  Array with population. Used only with `yll.r` and `asyr` types.

- w:

  Array with population weights for direct standardization. Used only
  with `asyr` type.

- standard:

  Data frame. User-specific standard life expectancy to calculate YLL
  with the following columns: age, ex. Note: the `age.int` argument
  should be consistent with the `age` column in this data frame.

## Value

list with values.

## Details

Computes four types of Years of Life Lost (YLL) indicators:

- **Absolute YLL** (`type = "yll"`): \$\$YLL\_{x,t,c} = D\_{x,t,c}
  \times SLE_x\$\$ where \\x,t,c\\ are age, time, and cause
  respectively, \\D\_{x,t,c}\\ is deaths in age \\x\\ at time \\t\\ from
  cause \\c\\ and \\SLE_x\\ is standard life expectancy at age \\x\\

- **YLL proportion** (`type = "yll.p"`): \$\$YLL\_{x,t,c}^p =
  \frac{YLL\_{x,t,c}}{YLL\_{x,t}}\$\$ where \\YLL\_{x,t} = \sum\_{c}
  YLL\_{x,t,c}\\ (total YLL across causes)

- **YLL rate** (`type = "yll.r"`): \$\$YLL\_{x,t,c}^r = \left(
  \frac{YLL\_{x,t,c}}{N\_{x,t}} \right) \times 100'000\$\$ where
  \\N\_{x,t}\\ is population in age group \\x\\ at time \\t\\

- **Age-standardized YLL rate** (`type = "asyr"`): \$\$ASYR\_{t,c} =
  \sum\_{x=\alpha}^{\omega} \left( YLL\_{x,t,c}^r \times w_x \right)\$\$
  where \\\alpha\\ to \\\omega\\ corresponds to first to last age group,
  \\w_x\\ is standard population weight for age \\x\\.

## References

Martinez, R., Soliz, P., Caixeta, R., & Ordunez, P. (2019). Reflection
on modern methods: years of life lost due to premature mortality—a
versatile and comprehensive measure for monitoring non-communicable
disease mortality. *International Journal of Epidemiology*, *48*,
1367–1376.
