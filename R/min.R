#' sat_min
#'
#' returns an ad-hoc measure of accuracy for the binning procedure which may be minimised.
#'
#'@param e a vector of (center - mean)^2 one value for each bin
#'@param kurtosis a vector of kurtosis values for each bin
#'
#'@return a value to be minimised
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_min <- function(e) {

  l <- length(e)
  s <- sum(e)

  return(l * s)
}
