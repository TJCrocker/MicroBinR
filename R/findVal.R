#' findVal
#'
#' applies a function to each bin returning a value for each fragment reading
#'
#'@param frag a vector of microsatilite fragment length
#'@param lim A vector of bin limits
#'@param FUN a function to be applied to each bin returning a value for each fragment
#'@param ... additional arguments to be supplied to FUN

#'@return a list containing results of bandwidth estimation, KDE, bin limits, central values, mean and frequency.
#'
#'@export
#'


findVal <- function(frag, lim, FUN, ...) {

  n <- seq_along(lim[-1])

  arg <- list(...)

  result <- purrr::map(n, ~{

    index <- frag > lim[.] & frag < lim[.+1]

    return(FUN(frag[index], center = arg[[1]][.]))

  })

  return(unlist(result))
}
