#' Get data from RosBris (DEPRECATED)
#'
#' @param type Character. Type of data for downloading. "m" = mortality, f = "ferility"
#' @param age Numeric. Age group. `1` for one-year data, `5` for five-year data
#' @param initial Logical. Do you want initial rosbris data? `T` for initial, `F` for 'long' format (default).
#' @param lastyear Numeric. The last available year in rosbris
#' (can be seen on the [website](http://demogr.nes.ru/index.php/ru/demogr_indicat/data)). Default is `2022` (in 2023).
#'
#' @references Center for Demographic Research (2023). *Russian Fertility and Mortality Database (Rosbris)*. [URL](http://www.demogr.nes.ru/en/demogr_indicat/data)
#' @return List (if `initial = TRUE`) of dataframes: population and mortality/fertility data. Dataframe (if `initial = FALSE`) in long format.
#' @import dplyr
#' @import tidyr
#' @export
get_rosbris <- function(type, age = 1, initial = FALSE, lastyear = 2022){

  stop("
       Recently RosBris has been switched to new website, so the function `get_rosbris` does not work now. I hope in a several months all mistakes due to new website will be fixed and function will work.
       Now RosBris data is presented in the `demor` as datasets in the long-format.
       use demor::rosbris_fertility_pop_1 (fertility, 1-year + Pop + Births)
       use demor::rosbris_fertility_pop_5 (fertility, 5-year + By parity + Pop + Births)
       use demor::rosbris_mortality_pop_1 (mortality, 1-year + Pop + Deaths)
       use demor::rosbris_mortality_pop_5 (mortality, 5-year + Pop + Deaths)

       ")

  if(type != "m" & type != "f"){
    stop("choose type of returned data: `m` (mortality) or `f` (fertility)")
  }
  else {
    if (age !=1 & age !=5){
      stop("choose age: 1 or 5")
    } else {
      agep <- ifelse(age==1,"","5")
    }
    if(type == "m"){
      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/DR",agep,"a1989-2014.zip"),
                    temp)
      data1m <- read.csv(unz(temp, paste0("DR",agep,"a1989-2014.txt")))
      unlink(temp)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/DR", agep, "a2015-", lastyear, ".zip"),
                    temp)
      fl = paste0("DR",agep,"a2015-", lastyear, ".txt")
      data2m <- read.csv(unz(temp, fl))
      unlink(temp)

      datam <- rbind(data1m,data2m)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/PopD",agep,"a1989-2014.zip"),
                    temp)
      data1p <- read.csv(unz(temp, paste0("PopD",agep,"a1989-2014.txt")))
      unlink(temp)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/PopD", agep,"a2015-", lastyear, ".zip"),
                    temp)
      fl = paste0("PopD", agep, "a2015-", lastyear, ".txt")
      data2p <- read.csv(unz(temp, fl))
      unlink(temp)

      datap <- rbind(data1p,data2p)

      if(initial){
        return(list(D = datam, Pop = datap))
      } else {
        datap <- datap %>%
          tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age",
                              values_to = "N") %>%
          dplyr::transmute (year=Year, code=Reg, sex=Sex, age, N, territory = Group) %>%
          dplyr::mutate (territory = tolower(territory), sex = tolower(sex)) %>%
          dplyr::mutate (sex = tolower(sex)) %>%
          dplyr::mutate (age = gsub(ifelse(agep=="","PopDa","PopD5a"),"", age)
          ) %>%
          dplyr::mutate (year = as.numeric(year), code = as.numeric(code),
                         N = as.numeric(N), age = as.numeric(age))

        datam <- datam %>%
          tidyr::pivot_longer(cols = dplyr::starts_with("Dr"), names_to = "age",
                              values_to = "mx") %>%
          transmute (year=Year, code=Reg, territory=Group, sex=Sex, age, mx) %>%
          mutate (territory = tolower(territory), sex = tolower(sex)) %>%
          dplyr::mutate (age = gsub(ifelse(agep=="","Dra","DrAa"),"", age)
          ) %>%
          mutate (year = as.numeric(year), code = as.numeric(code),
                  mx = as.numeric(mx), age = as.numeric(age)) %>%
          mutate (mx = mx / 1000000)

        data <- merge(datam, datap) %>%
          dplyr::mutate (Dx = round(mx * N,2))
        data$age <- as.numeric(data$age)
        data <- data %>% dplyr::arrange (code, year, territory, sex, age) %>% drop_na(code)

        rosbris.codes <- demor::rosbris.codes
        data <- left_join(data, rosbris.codes[,-1], by = "code")
        rm(rosbris.codes)

        return(data)
      }
    }
    else {
      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/BR",agep,"a1989-2014.zip"),
                    temp)
      data1f <- read.csv(unz(temp, paste0("BR",agep,"a1989-2014.txt")))
      unlink(temp)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/BR", agep, "a2015-", lastyear, ".zip"),
                    temp)
      fl = paste0("BR",agep,"a2015-", lastyear, ".txt")
      data2f <- read.csv(unz(temp, fl))
      unlink(temp)

      dataf <- rbind(data1f,data2f)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/PopB",agep,"a1989-2014.zip"),
                    temp)
      data1p <- read.csv(unz(temp, paste0("PopB",agep,"a1989-2014.txt")))
      unlink(temp)

      temp <- tempfile()
      download.file(paste0("http://demogr.nes.ru/images/uploads/PopB", agep,"a2015-", lastyear, ".zip"),
                    temp)
      fl = paste0("PopB", agep, "a2015-", lastyear, ".txt")
      data2p <- read.csv(unz(temp, fl))
      unlink(temp)

      datap <- rbind(data1p,data2p)

      if(initial){
        return(list(B = dataf, Pop = datap))
      } else {
        datap <- datap %>%
          tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age",
                              values_to = "N") %>%
          dplyr::transmute (year=Year, code=Reg, age, N, territory = Group) %>%
          dplyr::mutate (territory = tolower(territory)) %>%
          dplyr::mutate (age = gsub(ifelse(agep=="","PopBa","PopB5a"),"", age)
          ) %>%
          dplyr::mutate (year = as.numeric(year), code = as.numeric(code),
                         N = as.numeric(N), age = as.numeric(age))

        dataf <- dataf %>%
          tidyr::pivot_longer(cols = dplyr::starts_with("Br"), names_to = "age",
                              values_to = "fx") %>%
          transmute (year=Year, code=Reg, territory=Group, age, fx) %>%
          mutate (territory = tolower(territory)) %>%
          dplyr::mutate (age = gsub(ifelse(agep=="","Bra","Br5a"),"", age)
          ) %>%
          mutate (year = as.numeric(year), code = as.numeric(code),
                  fx = as.numeric(fx), age = as.numeric(age)) %>%
          mutate (fx = fx / 1000000)

        data <- merge(dataf, datap) %>%
          dplyr::mutate (Bx = round(fx * N,2))
        data$age <- as.numeric(data$age)
        data <- data %>% dplyr::arrange (code, year, territory, age) %>% drop_na(code)

        rosbris.codes <- demor::rosbris.codes
        data <- left_join(data, rosbris.codes[,-1], by = "code")
        rm(rosbris.codes)

        return(data)
      }
    }
  }
}
