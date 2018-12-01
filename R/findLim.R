#' findLim
#'
#' finds the bin limits based on the estimated distribution of fragment lengths
#'
#'@param x x component of distribution
#'@param y y component of ditribution
#'@param range the number of sequential x values to use to detect a bin limit (units of 0.01 bases)
#'
#'@return a vector of bin limits
#'
#'@export
#'

findLim <- function (x, y, range = 1L) {

  l <- length(x)

  eps <- .Machine$double.eps * l

  results <- list(min(x))

  i <- 1L + range

  repeat{

    if (all(
      !y[i - range] < y[i] | abs(y[i - range] - y[i]) < eps,
      y[i + range] > y[i] & abs(y[i + range] - y[i]) > eps
    )) {
      results <- list(results, x[i])
    }

    if(i == (l-range)) {
      results <- list(results, x[i])

      break
    } else {
      i <- i+1L
    }
  }

  return(unlist(results))

}
