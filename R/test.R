#' test
#'
#' Produces a unwighted KDE estimation useing score for a given bandwidth and then applies an error function to the resulting diistribution.
#'
#'@param h bandwidth
#'@param frag a vector of fragment lengths
#'@param FUN an error function
#'
#'@return a doubble produced by the error function
#'
#'@export
#'

test <- function(h, frag, FUN) {

  kde <- score(frag, h = h)

  return(FUN(kde$x, kde$y))

}
