#' Data on mortality of US men in 2002 by some causes
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{age}{age}
#'   \item{neoplasms}{age specific mortality rate (asmr) from Neoplasms}
#'   \item{circulatory}{asmr from Diseases of the circulatory system}
#'   \item{respiratory}{asmr from Diseases of the respiratory system}
#'   \item{digestive}{asmr from Diseases of the digestive system}
#'   \item{other}{asmr from other causes}
#'   \item{all}{overall asmr}
#' }
#' @source Andreev & Shkolnikov spreadsheet, can be seen on this [webpage](https://www.demogr.mpg.de/en/publications_databases_6118/publications_1904/mpidr_technical_reports/an_excel_spreadsheet_for_the_decomposition_of_a_difference_between_two_values_of_an_aggregate_demographic_4591)
"asdtex"

#' Data on mortality of US and England and Wales men in 2002 by some causes
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{age}{age}
#'   \item{neoplasms}{age specific mortality rate (asmr) from Neoplasms}
#'   \item{circulatory}{asmr from Diseases of the circulatory system}
#'   \item{respiratory}{asmr from Diseases of the respiratory system}
#'   \item{digestive}{asmr from Diseases of the digestive system}
#'   \item{other}{asmr from other causes}
#'   \item{all}{overall asmr}
#'   \item{cnt}{country: `usa` - US, `eng` - England and Wales}
#' }
#' @source Andreev & Shkolnikov spreadsheet, can be seen on this [webpage](https://www.demogr.mpg.de/en/publications_databases_6118/publications_1904/mpidr_technical_reports/an_excel_spreadsheet_for_the_decomposition_of_a_difference_between_two_values_of_an_aggregate_demographic_4591)
"mdecompex"

#' Data on Standard Life expectancies that is used for YLL calculations
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{stand}{Standard: 1 - World Health Organization Standard Life Expectancy by single-age; 2 - Global Burden of Disease studies (GBD) and WHO Global Health Estimates (WHO GHE) Standard Life Expectancy by 5-year age groups.}
#'   \item{age}{age}
#'   \item{ex}{Standard Life Expectancy}
#' }
#' @source Martinez, R., Soliz, P., Caixeta, R., Ordunez, P. [2019](https://doi.org/10.1093/ije/dyy254). Reflection on modern methods: years of life lost due to premature mortality—a versatile and comprehensive measure for monitoring non-communicable disease mortality. International Journal of Epidemiology 48, 1367–1376.
"sle_stand"

#' Rosbris' Region codes
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{n}{number of Region (sequence number)}
#'   \item{name}{Region name}
#'   \item{code}{Region unique code}
#' }
#' @source Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at https://www.nes.ru/demogr-fermort-data
"rosbris.codes"

#' Rosbris' Fertility and population (1-year interval)
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{year}{year}
#'   \item{code}{number of Region}
#'   \item{territory}{rural/urban/total}
#'   \item{age}{age}
#'   \item{fx}{ASFR}
#'   \item{N}{Population}
#'   \item{Bx}{Number of Births}
#'   \item{name}{Region name}
#' }
#' @source Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at https://www.nes.ru/demogr-fermort-data
"rosbris_fertility_pop_1"

#' Rosbris' Fertility and population (5-year interval)
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{year}{year}
#'   \item{code}{number of Region}
#'   \item{territory}{rural/urban/total}
#'   \item{age}{age}
#'   \item{fx}{ASFR}
#'   \item{fx1}{ASFR, 1 order}
#'   \item{fx2}{ASFR, 2 order}
#'   \item{fx3}{ASFR, 3 order}
#'   \item{fx4}{ASFR, 4 order}
#'   \item{fx5}{ASFR, 5+ order}
#'   \item{N}{Population}
#'   \item{Bx}{Number of Births}
#'   \item{Bx1}{Number of Births, 1 order}
#'   \item{Bx2}{Number of Births, 2 order}
#'   \item{Bx3}{Number of Births, 3 order}
#'   \item{Bx4}{Number of Births, 4 order}
#'   \item{Bx5}{Number of Births, 5+ order}
#'   \item{name}{Region name}
#' }
#' @source Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at https://www.nes.ru/demogr-fermort-data
"rosbris_fertility_pop_5"

#' Rosbris' Mortality and population (1-year interval)
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{year}{year}
#'   \item{code}{number of Region}
#'   \item{territory}{rural/urban/total}
#'   \item{sex}{sex: both/male/femal}
#'   \item{age}{age}
#'   \item{mx}{ASMR}
#'   \item{N}{Population}
#'   \item{Dx}{Number of Deaths}
#'   \item{name}{Region name}
#' }
#' @source Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at https://www.nes.ru/demogr-fermort-data
"rosbris_mortality_pop_1"

#' Rosbris' Mortality and population (5-year interval)
#'
#'
#' @format ## A dataframe
#' \describe{
#'   \item{year}{year}
#'   \item{code}{number of Region}
#'   \item{territory}{rural/urban/total}
#'   \item{sex}{sex: both/male/femal}
#'   \item{age}{age}
#'   \item{mx}{ASMR}
#'   \item{N}{Population}
#'   \item{Dx}{Number of Deaths}
#'   \item{name}{Region name}
#' }
#' @source Russian Fertility and Mortality Database. Center for Demographic Research, Moscow (Russia). Available at https://www.nes.ru/demogr-fermort-data
"rosbris_mortality_pop_5"



