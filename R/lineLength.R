#' lineLength
#'
#' finds the length of a line drawn between a series of sequential points described by x and y coordinates
#'
#'@param x a vector of x coordinates
#'@param y a vector of y coordinates
#'
#'@return the length of the line
#'
#'@export
#'

lineLength <- function (x, y) {

  n <- seq_along(x[-1])

  hyp <- purrr::map_dbl(n, ~sqrt(abs((y[.] - y[.+1])^2) + 1e-04))

  return(sum(hyp))

}
