#' Download RosBris Data
#'
#' Download age-specific mortality, fertility, and population data from the
#' Russian Fertility and Mortality Database (RosBris) and return them as
#' long-format data frames for use in `demor`.
#'
#' @param dataset Character. Dataset to download and parse. One of
#'   `"mortality_1"`, `"mortality_5"`, `"fertility_1"`, or `"fertility_5"`.
#' @param refresh Logical. If `FALSE` (default), previously downloaded RosBris
#'   archives are reused from the local cache directory. If `TRUE`, archives are
#'   downloaded again from the RosBris website.
#'
#' @details
#' The function downloads official `.zip` archives from the RosBris website,
#' stores them in a user cache directory created by `tools::R_user_dir()`, reads
#' `.txt` tables from the archives, and converts them to long-format data frames.
#'
#' Returned data have the following structure:
#'
#' - `"mortality_1"`: `year`, `code`, `territory`, `sex`, `age`, `mx`, `N`,
#'   `Dx`, `name`.
#' - `"mortality_5"`: `year`, `code`, `territory`, `sex`, `age`, `mx`, `N`,
#'   `Dx`, `name`.
#' - `"fertility_1"`: `year`, `code`, `territory`, `age`, `fx`, `N`, `Bx`,
#'   `name`.
#' - `"fertility_5"`: `year`, `code`, `territory`, `age`, `fx`, `fx1`, `fx2`,
#'   `fx3`, `fx4`, `fx5`, `N`, `Bx`, `Bx1`, `Bx2`, `Bx3`, `Bx4`, `Bx5`,
#'   `name`.
#'
#' At the moment, `get_rosbris()` uses the legacy RosBris series corresponding
#' to the periods `1989-2014` and `2015-2022`. Updated post-census series are
#' not used by this function.
#'
#' @return A data frame. Column structure depends on `dataset`; see **Details**.
#'
#' @seealso [rosbris.codes]
#'
#' @source Russian Fertility and Mortality Database. Center for Demographic
#'   Research, Moscow (Russia). Available at
#'   <https://www.nes.ru/research-main/research-centers/demogr/demogr-fermort-data>
#'
#' @examples
#' \dontrun{
#' mort <- get_rosbris("mortality_5")
#' fert <- get_rosbris("fertility_1")
#'
#' rus2010 <- subset(
#'   mort,
#'   year == 2010 & code == 1100 & sex == "m" & territory == "t"
#' )
#' }
#'
#' @export
get_rosbris <- function(dataset = c("mortality_1", "mortality_5", "fertility_1", "fertility_5"),
                        refresh = FALSE) {
  dataset <- match.arg(dataset)

  switch(
    dataset,
    mortality_1 = .rosbris_mortality_1(refresh = refresh),
    mortality_5 = .rosbris_mortality_5(refresh = refresh),
    fertility_1 = .rosbris_fertility_1(refresh = refresh),
    fertility_5 = .rosbris_fertility_5(refresh = refresh)
  )
}


.rosbris_zip_csv <- function(url, refresh = FALSE) {
  cache_dir <- file.path(tools::R_user_dir("demor", which = "cache"), "rosbris")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(cache_dir, basename(url))

  if (!file.exists(zip_path) || isTRUE(refresh)) {
    utils::download.file(
      url = url,
      destfile = zip_path,
      mode = "wb",
      quiet = TRUE
    )
  }

  members <- utils::unzip(zip_path, list = TRUE)
  txt_member <- members$Name[grepl("\\.txt$", members$Name, ignore.case = TRUE)][1]

  con <- unz(zip_path, txt_member)
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  utils::read.csv(con, check.names = FALSE)
}


.rosbris_num <- function(x) {
  suppressWarnings(as.numeric(x))
}


.rosbris_codes <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("rosbris.codes", package = "demor", envir = env)
  env[["rosbris.codes"]]
}


.rosbris_add_names <- function(data) {
  codes <- .rosbris_codes()

  if (is.null(codes)) {
    return(data)
  }

  dplyr::left_join(data, codes[, c("code", "name")], by = "code")
}


.rosbris_extract_br5_from_order <- function(data_order) {
  keep <- c("Year", "Reg", "Group", grep("^BrOAa", names(data_order), value = TRUE))
  out <- data_order[, keep, drop = FALSE]
  names(out) <- sub("^BrOAa", "Br5a", names(out))
  out
}


.rosbris_mortality_1 <- function(refresh = FALSE) {
  data1m <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/02/001-1-DRa1989-2014.zip",
    refresh = refresh
  )
  data2m <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/DRa2015-2022.zip",
    refresh = refresh
  )
  datam <- rbind(data1m, data2m)

  data1p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/03/005-1-PopDa1989-2014.zip",
    refresh = refresh
  )
  data2p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/08/PopDa2015-2022.zip",
    refresh = refresh
  )
  datap <- rbind(data1p, data2p)

  datap <- datap %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age", values_to = "N") %>%
    dplyr::transmute(year = Year, code = Reg, sex = Sex, age, N, territory = Group) %>%
    dplyr::mutate(territory = tolower(territory), sex = tolower(sex)) %>%
    dplyr::mutate(age = gsub("PopDa", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      N = .rosbris_num(N),
      age = .rosbris_num(age)
    )

  datam <- datam %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Dr"), names_to = "age", values_to = "mx") %>%
    dplyr::transmute(year = Year, code = Reg, territory = Group, sex = Sex, age, mx) %>%
    dplyr::mutate(territory = tolower(territory), sex = tolower(sex)) %>%
    dplyr::mutate(age = gsub("Dra", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      mx = .rosbris_num(mx),
      age = .rosbris_num(age)
    ) %>%
    dplyr::mutate(mx = mx / 1000000)

  data <- merge(datam, datap) %>%
    dplyr::mutate(Dx = round(mx * N, 2))

  data$age <- .rosbris_num(data$age)
  data <- data %>%
    dplyr::arrange(code, year, territory, sex, age) %>%
    tidyr::drop_na(code)

  .rosbris_add_names(data)
}


.rosbris_mortality_5 <- function(refresh = FALSE) {
  data1m <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/02/003-1-DR5a1989-2014.zip",
    refresh = refresh
  )
  data2m <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/DR5a2015-2022.zip",
    refresh = refresh
  )
  datam <- rbind(data1m, data2m)

  data1p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/03/007-1-PopD5a1989-2014.zip",
    refresh = refresh
  )
  data2p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/PopD5a2015-2022.zip",
    refresh = refresh
  )
  datap <- rbind(data1p, data2p)

  datap <- datap %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age", values_to = "N") %>%
    dplyr::transmute(year = Year, code = Reg, sex = Sex, age, N, territory = Group) %>%
    dplyr::mutate(territory = tolower(territory), sex = tolower(sex)) %>%
    dplyr::mutate(age = gsub("PopD5a", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      N = .rosbris_num(N),
      age = .rosbris_num(age)
    )

  datam <- datam %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Dr"), names_to = "age", values_to = "mx") %>%
    dplyr::transmute(year = Year, code = Reg, territory = Group, sex = Sex, age, mx) %>%
    dplyr::mutate(territory = tolower(territory), sex = tolower(sex)) %>%
    dplyr::mutate(age = gsub("DrAa", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      mx = .rosbris_num(mx),
      age = .rosbris_num(age)
    ) %>%
    dplyr::mutate(mx = mx / 1000000)

  data <- merge(datam, datap) %>%
    dplyr::mutate(Dx = round(mx * N, 2))

  data$age <- .rosbris_num(data$age)
  data <- data %>%
    dplyr::arrange(code, year, territory, sex, age) %>%
    tidyr::drop_na(code)

  .rosbris_add_names(data)
}


.rosbris_fertility_1 <- function(refresh = FALSE) {
  data1f <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/01/02-1-BRa1989-2014.zip",
    refresh = refresh
  )
  data2f <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/BRa2015-2022.zip",
    refresh = refresh
  )
  dataf <- rbind(data1f, data2f)

  data1p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/03/010-1-PopBa1989-2014.zip",
    refresh = refresh
  )
  data2p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/PopBa2015-2022.zip",
    refresh = refresh
  )
  datap <- rbind(data1p, data2p)

  datap <- datap %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age", values_to = "N") %>%
    dplyr::transmute(year = Year, code = Reg, age, N, territory = Group) %>%
    dplyr::mutate(territory = tolower(territory)) %>%
    dplyr::mutate(age = gsub("PopBa", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      N = .rosbris_num(N),
      age = .rosbris_num(age)
    )

  dataf <- dataf %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Br"), names_to = "age", values_to = "fx") %>%
    dplyr::transmute(year = Year, code = Reg, territory = Group, age, fx) %>%
    dplyr::mutate(territory = tolower(territory)) %>%
    dplyr::mutate(age = gsub("Bra", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      fx = .rosbris_num(fx),
      age = .rosbris_num(age)
    ) %>%
    dplyr::mutate(fx = fx / 1000000)

  data <- merge(dataf, datap) %>%
    dplyr::mutate(Bx = round(fx * N, 2))

  data$age <- .rosbris_num(data$age)
  data <- data %>%
    dplyr::arrange(code, year, territory, age) %>%
    tidyr::drop_na(code)

  .rosbris_add_names(data)
}


.rosbris_fertility_5 <- function(refresh = FALSE) {
  data1f <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/01/05-1-BR5a1989-2014.zip",
    refresh = refresh
  )
  data1fp <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/01/07-1-BR5aO1989-2014.zip",
    refresh = refresh
  )
  data2fp <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/BR5aO2015-2022.zip",
    refresh = refresh
  )

  data2f <- .rosbris_extract_br5_from_order(data2fp)
  dataf <- rbind(data1f, data2f)
  datafp <- rbind(data1fp, data2fp)

  data1p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/03/012-1-PopB5a1989-2014.zip",
    refresh = refresh
  )
  data2p <- .rosbris_zip_csv(
    "https://www.nes.ru/files/research/demogr/data-rus/05/PopB5a2015-2022.zip",
    refresh = refresh
  )
  datap <- rbind(data1p, data2p)

  datap <- datap %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Pop"), names_to = "age", values_to = "N") %>%
    dplyr::transmute(year = Year, code = Reg, age, N, territory = Group) %>%
    dplyr::mutate(territory = tolower(territory)) %>%
    dplyr::mutate(age = gsub("PopB5a", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      N = .rosbris_num(N),
      age = .rosbris_num(age)
    )

  dataf <- dataf %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Br"), names_to = "age", values_to = "fx") %>%
    dplyr::transmute(year = Year, code = Reg, territory = Group, age, fx) %>%
    dplyr::mutate(territory = tolower(territory)) %>%
    dplyr::mutate(age = gsub("Br5a", "", age)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      fx = .rosbris_num(fx),
      age = .rosbris_num(age)
    ) %>%
    dplyr::mutate(fx = fx / 1000000)

  datafp <- datafp %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Br"), names_to = "age", values_to = "fx") %>%
    dplyr::transmute(year = Year, code = Reg, territory = Group, age, fx) %>%
    dplyr::mutate(territory = tolower(territory)) %>%
    dplyr::mutate(age = gsub("BrO", "", age)) %>%
    dplyr::mutate(age = gsub("a", "", age)) %>%
    dplyr::mutate(parity = substring(age, 1, 1), age = substring(age, 2)) %>%
    dplyr::mutate(
      year = .rosbris_num(year),
      code = .rosbris_num(code),
      fx = .rosbris_num(fx),
      age = .rosbris_num(age)
    ) %>%
    dplyr::mutate(fx = fx / 1000000) %>%
    dplyr::mutate(parity = paste0("fx", parity)) %>%
    tidyr::pivot_wider(names_from = parity, values_from = fx) %>%
    dplyr::select(-fxA)

  dataf <- dplyr::left_join(dataf, datafp, by = c("code", "age", "territory", "year"))

  data <- dplyr::left_join(dataf, datap, by = c("code", "age", "territory", "year")) %>%
    dplyr::mutate(Bx = round(fx * N, 2))

  data$age <- .rosbris_num(data$age)
  data <- data %>%
    dplyr::arrange(code, year, territory, age) %>%
    tidyr::drop_na(code)

  for (i in 1:5) {
    data[[paste0("Bx", i)]] <- round(data[[paste0("fx", i)]] * data[["N"]], 2)
  }

  .rosbris_add_names(data)
}
