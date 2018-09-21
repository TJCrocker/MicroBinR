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

sat_findLimits <- function (z, y, range = 20) {

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

  return(results)
}


#sat_findLimtis1 <- function (z, y, range = 20) {
 # l <- seq_along(z)[range:(length(z) - range)]


  #results <- purrr::map_dbl(l, function (l) {

   # if (all(

      # all values to the right and left are decending or
      #   acending respectively
    #  all(y[l:(l - range)] == sort(y[(l - range):l])),
     # all(y[l:(l + range)] == sort(y[l:(l + range)]))


    #)) {

     # return(z[l])

  #  } else {

   #   return(NA)
    #}

  #})

  #results <- results[!is.na(results)]
  #return(results)
  #}




