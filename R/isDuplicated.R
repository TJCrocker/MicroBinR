#'isDuplicated
#'
#' isDuplicated takes an object that is coercable to a matrix and returns a tibble of logical indicating
#'  wheather values for each observation are duplicates. The first column is exspected to be a primary key
#'  so it is returned unchanged.
#'
#'@param x an object that is coerceable to a matrix containing raw SSR fragment lengths.
#'
#'@return a complex named list containing an element for each locus in the input data
#' and a sub-element for the output of each of SSRr's core functions
#'
#'@export

# function ----------------------------------------------------------------------------------------------

isDuplicated <- function (data) {

  # Find names ----
  names <- names(data)

  # retrive IDs ----
  id <- data[1]

  # Coerce data columns to matrix ----
  x <- as.matrix(data[-1])

  # Find the length and width of the input data ----
  l <- length(x[,1])
  w <- length(x[1,])

  # Build out file: a logical with same dims as input minus id variable
  dup <- matrix(NA, l, w)

  # Test for duplicates ----
  for (i in 1:l) {
    dup[i,] <- duplicated(x[i,])

  }

  # Re-attach ID and names then coerce to tibble ----
  dup <- tibble::as_tibble(cbind(id, dup))
  names(dup) <- names

  return(dup)

}
