#' Split continuous ages to age groups
#'
#' @param x Array with numeric values of age
#' @param groups Array with numeric values of lower boundaries of age groups
#' @param char Logical. Should output array be numeric (FALSE) or character (Factor) array (TRUE)
#' @return Array with classified ages
#' @examples
#' groups <- c(0,1,5,10,15,20, 24, 45, 85)
#' x <- sample(0:100, 101)
#' ages(x, groups, FALSE)
#'
#' @export
ages <- function(x, groups, char = FALSE){
  if(char == FALSE){
    for(i in 1:length(groups)){
      if(i == 1){
        gr <- ifelse(x <= groups[i], groups[i], x)
      }else if (i != length(groups)){
        gr <- ifelse(gr >= groups[i] & gr < groups[i+1], groups[i], gr)
      }else {
        gr <- ifelse(gr >= groups[i], groups[i], gr)
      }
    }
    return(gr)
  }else{

    for(i in 1:length(groups)){
      if(i == 1){
        if(groups[1] == 0){
          ch <- "0"
        }else{
          ch <- paste0("<",groups[1])
        }
      } else if (i != length(groups)){
        ch <- c(ch, paste0(groups[i],"-",groups[i+1]-1))
      }else{
        ch <- c(ch, paste0(groups[i],"+"))
      }
    }
    for(i in 1:length(groups)){
      if(i == 1){
        gr <- ifelse(x <= groups[i], ch[i], x)
      }else if (i != length(groups)){
        gr <- ifelse(x >= groups[i] & x < groups[i+1], ch[i], gr)
      }else{
        gr <- ifelse(x >= groups[i], ch[i], gr)
      }
    }
    return(factor(gr, levels = ch))
  }
}
