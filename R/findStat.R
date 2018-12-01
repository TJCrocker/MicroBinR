#' findStat
#'
#' returns descriptive statistics about the raw fragment leanghts in each bin
#'
#'@param frag vector of raw fragment lengths
#'@param lim a vector of bin limits
#'@param FUN a function to be applied the fragments within each bin
#'
#'@return a vector of doubbles
#'
#'@export
#'


findStat <- function (frag, lim, FUN) {

  stopifnot(is.function(FUN))

  n <- 1:(length(lim) - 1)

  stat <- purrr::map_dbl(n, ~FUN(frag[frag < lim[.+1] & frag > lim[.]]))

  return(stat)

}
