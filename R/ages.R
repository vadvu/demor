#' Split continuous ages to age groups
#'
#' @param x Numeric array. Values of age that should be grouped
#' @param groups Numeric array. Values of lower boundaries of age groups
#' @param char Logical. Should output be numeric (`FALSE`) or character (`TRUE`)? By default, `FALSE`
#' @param below_min_val What value to return for x < min(groups) when char = `FALSE`? Default is NA.
#' @return Factor or numeric array with classified ages
#' @examples
#' groups <- c(0, 1, 5, 10, 15, 20, 24, 45, 85)
#' x <- 0:100
#' ages(x, groups, TRUE)
#'
#' @export
ages <- function(x, groups, char = FALSE, below_min_val = NA) {

  breaks_full <- c(-Inf, groups, Inf)

  res_cut <- cut(x, breaks = breaks_full, right = FALSE)

  if(char){
    lab_below <- paste0("<", groups[1])

    starts <- groups[-length(groups)]
    ends <- groups[-1] - 1 #so it works only with integers

    lab_mids <- paste0(starts, "-", ends)
    lab_mids <- ifelse(lab_mids == "0-0", "0-1", lab_mids)

    lab_last <- paste0(groups[length(groups)], "+")

    all_labels <- c(lab_below, lab_mids, lab_last)
    levels(res_cut) <- all_labels

    return(res_cut)

  }else{

    map_values <- c(below_min_val, groups)

    return(map_values[as.integer(res_cut)])

  }
}
