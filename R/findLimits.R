#' sat_findLimits
#'
#' sat_findLimits finds z values where "range" zvalues in either direction increase and decrease consecutively
#'
#'@param z a vector of possible fragment lengths
#'@param y a vector of desity of fragment lengths, must be same length as z
#'@param range the number of z values consecutively riseing or and falling in either direction.
#'
#'@return A list of z values
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_findLimits <- function (z, y, range = 2) {

   l <- length(z)

  results <- list(0)

  for(i in range:(l-range)) {

    #print(bin)

    if (all(

      # all values to the right and left are decending or
      #   acending respectively
      all(y[i:(i - range)] == sort(y[(i - range):i])),
      all(y[i:(i + range)] == sort(y[i:(i + range)]))


    )) {

      results <- list(results, z[i])

    }

  }

  results <- list(results, max(z))

  results <- unlist(results)

  results[1] <- min(z) - 1

  return(results)
}





