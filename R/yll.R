#' Years of Life Lost (YLL) calculation
#'
#' @param Dx Array with the number of deaths.
#' @param type Character. Type of YLL to calculate. See details section.
#' @param age.int Numeric. Age interval of Dx. Can be `1` or `5`.
#' @param Dx_all Array with the number of all deaths. Used only with `yll.p` type, where `Dx` is array with cause-specific deaths.
#' @param pop Array with population. Used only with `yll.r` and `asyr` types.
#' @param w Array with population weights for direct standardization. Used only with `asyr` type.
#' @param standard Data frame. User-specific standard life expectancy to calculate YLL with the following columns: age, ex. Note: the `age.int` argument should be consistent with the `age` column in this data frame.
#' @details
#' Computes four types of Years of Life Lost (YLL) indicators:
#' \itemize{
#'   \item \strong{Absolute YLL} (\code{type = "yll"}):
#'     \deqn{YLL_{x,t,c} = D_{x,t,c} \times SLE_x}
#'     where \eqn{x,t,c} are age, time, and cause respectively, \eqn{D_{x,t,c}} is deaths in age \eqn{x} at time \eqn{t} from cause \eqn{c} and \eqn{SLE_x} is standard life expectancy at age \eqn{x}
#'   \item \strong{YLL proportion} (\code{type = "yll.p"}):
#'     \deqn{YLL_{x,t,c}^p = \frac{YLL_{x,t,c}}{YLL_{x,t}}}
#'     where \eqn{YLL_{x,t} = \sum_{c} YLL_{x,t,c}} (total YLL across causes)
#'
#'   \item \strong{YLL rate} (\code{type = "yll.r"}):
#'     \deqn{YLL_{x,t,c}^r = \left( \frac{YLL_{x,t,c}}{N_{x,t}} \right) \times 100'000}
#'     where \eqn{N_{x,t}} is population in age group \eqn{x} at time \eqn{t}
#'
#'   \item \strong{Age-standardized YLL rate} (\code{type = "asyr"}):
#'     \deqn{ASYR_{t,c} = \sum_{x=\alpha}^{\omega} \left( YLL_{x,t,c}^r \times w_x \right)}
#'     where \eqn{\alpha} to \eqn{\omega} corresponds to first to last age group, \eqn{w_x} is standard population weight for age \eqn{x}.
#' }
#' @references
#' Martinez, R., Soliz, P., Caixeta, R., & Ordunez, P. (2019). Reflection on modern methods: years of life lost due to premature mortality—a versatile and comprehensive measure for monitoring non-communicable disease mortality. *International Journal of Epidemiology*, *48*, 1367–1376.
#' @return list with values.
#' @export
#'
yll <- function(Dx, type = c("yll", "yll.p", "yll.r", "asyr"), age.int = 5, Dx_all = NULL, pop = NULL, w = NULL, standard = NULL){

  `%notin%` <- Negate(`%in%`)

  ndx = length(Dx)
  ndx_o = ndx
  if(is.null(standard)){
    sle <- demor::sle_stand
  } else {
    sle <- standard %>% mutate(stand = 1) %>% arrange(age)
  }


  if(age.int %notin% c(1,5)){
    stop("Age interval (age.int) should be 1 or 5")
  }
  if(type %notin% c("yll", "yll.p", "yll.r", "asyr")){
    stop("type should be one of the following: 'yll', 'yll.p', 'yll.r', 'asyr'")
  }

  if(age.int == 5){
    sle = sle[sle$stand==5,]$ex
    if(ndx != 19){
      excess = sum(Dx[length(sle):ndx])
      Dx <- Dx[1:length(sle)]
      ndx = length(Dx)
      Dx[ndx] = excess
    }
  }else{
    sle = sle[sle$stand==1,]$ex
    if(ndx > length(sle)){
      excess = sum(Dx[length(sle):ndx])
      Dx <- Dx[1:length(sle)]
      ndx = length(Dx)
      Dx[ndx] <- excess
    }
  }

  if(type == "yll"){

    return(
      list(
        yll_all = sum(sle[1:ndx] * Dx),
        yll = sle[1:ndx] * Dx
      )
    )

  } else if (type == "yll.p"){

    if(is.null(Dx_all)){
      stop("Cannot find Dx_all")
    }
    if(ndx_o != length(Dx_all)){
      stop("Lengths of Dx and Dx_all are different")
    }

    ndx_a <- length(Dx_all)
    if(ndx_a > length(sle)){
      excess = sum(Dx_all[length(sle):ndx_a])
      Dx_all <- Dx_all[1:length(sle)]
      ndx_a = length(Dx_all)
      Dx_all[ndx_a] <- excess
    }

    return(
      list(
        yll.p_all = ( sum(sle[1:ndx] * Dx) ) / sum(sle[1:ndx_a] * Dx_all),
        yll.p = ( sle[1:ndx] * Dx ) / ( sle[1:ndx_a] * Dx_all )
      )
    )

  } else if (type == "yll.r"){

    if(is.null(pop)){
      stop("Cannot find pop")
    }
    if(ndx_o != length(pop)){
      stop("Lengths of Dx and pop are different")
    }

    npx <- length(pop)
    if(npx > length(sle)){
      excess = sum(pop[length(sle):npx])
      pop <- pop[1:length(sle)]
      npx = length(pop)
      pop[npx] <- excess
    }

    return(
      list(
        yll.r_all = 100000 * sum(sle[1:ndx] * Dx)/sum(pop),
        yll.r = 100000 * (sle[1:ndx] * Dx) / pop
      )
    )

  }else if (type == "asyr"){

    if(is.null(pop)){
      stop("Cannot find pop")
    }
    if(ndx_o != length(pop)){
      stop("Lengths of Dx and pop are different")
    }
    if(length(pop) != length(w)){
      stop("Lengths of w and pop are different")
    }

    npx <- length(pop)
    if(npx > length(sle)){
      excess = sum(pop[length(sle):npx])
      pop <- pop[1:length(sle)]
      npx = length(pop)
      pop[npx] <- excess
    }

    nwx <- length(w)
    if(nwx > length(sle)){
      excess = sum(w[length(sle):nwx])
      w <- w[1:length(sle)]
      nwx = length(w)
      w[nwx] <- excess
    }

    return(
      list(
        asyr_all = sum( w * (100000 * (sle[1:ndx] * Dx) / pop) ),
        asyr =  w * (100000 * (sle[1:ndx] * Dx) / pop)
      )
    )
  }
}
