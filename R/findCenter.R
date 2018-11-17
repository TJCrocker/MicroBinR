#' sat_findCenter
#'
#' sat_findCenter finds the z with max y value for each bin defined by limits
#'
#'@param z a vector of possible fragment lengths
#'@param y a vector of desity of fragment lengths, must be same length as z
#'@param limits a vector of limits that can be used to partition z into bins.
#'
#'@return a central z value for each bin.
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_findCenter <- function (z, y, limits) {

  n <- 1:(length(limits) - 1)

  center <- purrr::map_dbl(n, ~{

    index <- z > limits[.] & z < limits[.+1]
    center <- z[index][y[index] == max(y[index])][[1]]
    return(center)

  })

  return(center)
}
