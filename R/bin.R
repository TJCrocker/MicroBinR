#' sat_bin
#'
#' Takes a vector of fragment lengths, lower limits and central values and sorts values into bins
#'
#'@param repo a database containing data for each loci (produced by sat_makeDatabase)
#'@param sgma a vector containing sigma values for each loci passed to sat_score (if null defaults to 0.4)
#'@param range a vector containing ranges for each loci passed to sat_binStats if (null defaults to 20)
#'
#'
#'@return vector of binned fragment lengths
#'
#'@export
#
# Function -------------------------------------------------------------------------------------------------------------

sat_bin <- function(frag_len, limits, center) {

  bin.num <- purrr::map_dbl(seq_along(frag_len), ~sum(dplyr::cumall(frag_len[.] >= limits)))

  out <- center[bin.num]

  return(out)

}

