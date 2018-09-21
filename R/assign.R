#' sat_assign
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

sat_assign <- function (repo) {

  nms <- names(repo)

  for (n in nms) {
    frag_len <- repo[[n]]$tidy$frag_len
    limits <- repo[[n]]$stat$lim.lower
    center <- repo[[n]]$stat$center

    frag_len.binned <- sat_bin(frag_len, limits, center)

    repo[[n]]$tidy <- cbind(repo[[n]]$tidy, frag_len.binned)
  }

  return(repo)

}
