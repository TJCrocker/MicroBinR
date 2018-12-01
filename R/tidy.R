#' tidy
#'
#' sat_tidy takes each locus from a the sat database, and puts it into tidy format. An additional duplicate column is added indicating whether raw fragment lengths are identical to fragment lengths befor them for a given locus and individual.
#'
#'@param data a dataframe like object where the first column is a unique identifier for each genotype and subseqent columns contain raw fragment lengths found for that locus.
#'
#'@return
#'
#'@export

# function ----------------------------------------------------------------------------------------------

tidy <- function (data) {

    # Test for duplicates ----
    dup <- isDuplicated(data)

    # Find ploidy and length of input ----
    p <- length(data) - 1
    l <- length(data[[1]])

    # Assemble into tibble ----
    tidy <-  tibble::tibble(
      id = rep(data[[1]], p),
      frag_len = c(as.matrix(data[-1])[,1:p]),
      allele = rep(colnames(data[-1]), each = l),
      duplicate = c(as.matrix(dup[-1])[,1:p])
    )


  return(tidy)

}
