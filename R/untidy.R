#' sat_untidy
#'
#' sat_untidy spreads fragment length data over multiple columns useing allele number as a key. opposite of sat_untidy.
#'
#'@param id each individuals unique identification
#'@param frag_len vector of fragment lengths
#'@param allele vector indicating which allele the fragment length represents
#'
#'@return a tibble containing a column for ID and a seporate column for each allele
#'
#'@export
#'
# function ---------------------------------------------------------------------------------------------

sat_untidy <- function(id, frag_len, allele) {

  alleles <- unique(allele)

  out <- list("id" = unique(id))

  for (i in seq_along(alleles)) {

    name <- as.character(alleles[i])
    out[[name]] <- frag_len[allele == alleles[i]]

  }

  out <- tibble::as_tibble(out)

  return(out)


}

