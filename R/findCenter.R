#' findCenter
#'
#' finds the mode of the distribution within each bin
#'
#'@param x x component of distribution
#'@param y y component of ditribution
#'@param lim a vector of bin limits
#'
#'@return a vector of central values
#'
#'@export
#'


findCenter <- function (x, y, lim) {

  n <- 1:(length(lim) - 1)

  center <- purrr::map_dbl(n, ~{

    index <- x > lim[.] & x < lim[.+1]
    center <- x[index][y[index] == max(y[index])][[1]]

    return(center)

  })

  return(center)
}
