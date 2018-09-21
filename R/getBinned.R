#' sat_getBinned
#'
#' sat_assign updates the sat repository with binned fragment lengths useing the initial data and bin limets produced by sat_bin.
#'
#'@param repo a repository initialised by sat_retrive and updated by sat_linerise, sat_score and sat_bin
#'
#'@return a complex named list containing an element for each locus in the input data
#' and a sub-element for the output of each of SSRr's core functions
#'
#'@export
#'
# function ---------------------------------------------------------------------------------------------

sat_getBinned <- function (repo) {

  # Get number of loci ----
  l <- length(repo)

  # Get number of alleles in at each loci and number of individuals ----
  ncol <- sum(purrr::map_dbl(1:l, ~repo[[.]]$info$ploidy)) + 1
  nrow <- length(repo[[1]]$data[[1]])

  # Build output object ----
  out <- tibble::as_tibble(matrix(NA, nrow = nrow, ncol = ncol))
  names_out <- vector("character", ncol)

  # Input identification information ----
  out[1] <- repo[[1]]$data[[1]]
  names_out[1] <- "id"

  for (i in 1:l) {

    # Get data for each locus ----
    id <- repo[[i]]$tidy$id
    frag_len <- repo[[i]]$tidy$frag_len.binned
    allele <- repo[[i]]$tidy$allele

    # spread alleles over multiple columns ----
    untdy <- sat_untidy(id, frag_len, allele)

    # get ploidy of locus (will need to be updated for differeing ploidy sizes) ----
    p <- repo[[i]]$info$ploidy

    # inout spread frag_len columns for locus ----
    out[,(((i - 1) * p) + 1):(i * p) + 1] <- untdy[-1]

    # name columns appropriately ----
    names_out[(((i - 1) * p) + 1):(i * p) + 1] <- c(names(repo)[i], stringr::str_c(names(repo[i]), "_", 1:(p-1)))

  }

  names(out) <- names_out

  return(out)
}
