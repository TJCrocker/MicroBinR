#' sat_score
#'
#' sat_score takes a vector of microsatilite raw fragment lengths and summs gausian distributions for
#'  each input produceing a combined distribution of the whole locus
#'
#'@param x a vector of raw fragment lengths
#'@param sigma assumed standard deviation of error for each raw fragment length
#'@param resolution the smallest detectable difference between raw fragment lengths
#'@param tail.length = the size of the buffer regions placed either side of raw fragment lengths
#'@param e an approximation of euilers constant
#'@param FUN an expression that is passed to purrr::map_dbl which is used to calculate y values
#'
#'@return a complex named list containing an element for each locus in the input data
#' and a sub-element for the output of each of SSRr's core functions
#'
#'@export


# function-------------------------------------------------------------------------------------------------

# do not deleat !!!!
sat_score <- function(x, h = 0.4, resolution = 0.01, tail.length = 1, K = NULL) {

  # If no function is provided default to gaussian ----
  if (is.null(K)) {
    K <- function(u, e = 2.71828182845905) {1 / (sqrt(2 * pi)) * e^ (-0.5 * (u^2))}
  } else {
    environment(K) <- environment()
  }

  # Compute z and y then assemble into tibble ----
  sco <- tibble::tibble(
    z = seq(min(x) - tail.length, max(x) + tail.length, by = resolution),
    y = purrr::map_dbl(1:length(z), function (.) {
      u <- (z[.] - x) / h
      out <- (length(x)^-1) * sum((h ^ -1) * K(u))
      return(out)
    })
  )

  return(sco)
}

#sat_score1 <- function(x, h = 0.4, resolution = 0.01, tail.length = 1, K = NULL) {

  # If no function is provided default to gaussian ----
 # if (is.null(K)) {
  #  K <- ~function(u, e = 2.71828182845905) {1 / (sqrt(2 * pi)) * e^ (-0.5 * (u^2))}
  #} else {
   # environment(K) <- environment()
  #}

  # Compute z and y then assemble into tibble ----
  #sco <- tibble::tibble(
   # z = seq(min(x) - tail.length, max(x) + tail.length, by = resolution),
    #y = purrr::map_dbl(z, function (.) {
     # u <- (z[.] - x) / h
      #y <- (length(x)^-1) * sum((h ^ -1) * K(u))
      #return(y)
    #})
    #y <- purrr::map_dbl(1:length(z), function (.) {
     # u <- (z[.] - x) / h
      #out <- (length(x)^-1) * sum((h ^ -1) * K.gauss(u))
     # return(out)
    #})

  #)

  #return(sco)
#}

#u <- ~(. - x) / h
#u <- (z[1] - x) / h

#K.gauss <- function(u, e = 2.71828182845905) {1 / (sqrt(2 * pi)) * e^ (-0.5 * (u^2))}
#K.gauss(u)

#y <- purrr::map_dbl(1:length(z), function (.) {
 # u <- (z[.] - x) / h
  #out <- (length(x)^-1) * sum((h ^ -1) * K.gauss(u))
  #return(out)
#})

#cbind(z, y) %>%
 # as.data.frame() %>%
#ggplot(aes(z, y)) +
 # geom_point()

#estimate <- ~(length(x)^-1) * sum((sgma ^ -1) * K.gauss(u))
