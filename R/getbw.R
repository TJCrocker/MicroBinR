#' getbw
#'
#' A wrapper for optim. Finds the optimum bandwidth by maximising the error lineLength.
#'
#'@param frag a vector of fragment lengths
#'
#'@return optimum bandwidth for the distribution of fragment lengths
#'
#'@export
#'

getbw <- function(frag) {

  bw <- optim(par = 0.4, fn = test, frag = frag, FUN = lineLength, control = list(fnscale = -1))

  return(bw)

}
