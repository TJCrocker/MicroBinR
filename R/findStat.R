#' sat_findStat
#'
#' sat_findStat sorts a vector of fragment lengths into bins and applies a function to each bin seporately.
#'
#'@param x a vector of raw fragment lengths
#'@param limits a vector of bin limits (range of limits must encompass range of x)
#'@param FUN the function to be applied to each bin (must produce single numeric for each bin)
#'
#'@return a numeric vector same length as number of bins (length(limits) - 1)
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_findStat <- function (x, limits, FUN = length) {

  n <- 1:(length(limits) - 1)

  stat <- purrr::map_dbl(n, ~FUN(x[x < limits[.+1] & x > limits[.]]))

  return(stat)

}