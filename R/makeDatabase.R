#' makeDatabase
#'
#' A function that builds a repository of named lists from a tidy input database for storing the outputs of other MicroBinR functions.
#'
#'@param df a data frame like object containing raw SSR fragment length data
#'@param ploidy  the number of alleles per locus per indiviual.
#'
#'@return a complex named list containing an element for each locus in the input data
#' and a sub-element for the output of each of SSRr's core functions
#'
#'@export

makeDatabase <- function(df, ploidy) {

  # Build database subunit ----
  sub <- list(
    list("complete" = FALSE, "h" = NULL, "range" = 20, "ploidy" = ploidy),
    list(),
    list()
  )
  names(sub) <- c("info", "data", "tidy")

  # Find locus names ----
  if (ploidy == 1) {
    locus_names <- names(df[-1])
  } else {
    locus_names <- names(df)[(seq_along(df) - 1) %% ploidy == 1]
  }


  # Build database ----
  repo <- rep(list(sub), length(locus_names))
  names(repo) <- locus_names

  # Populate data and convert to tidy format ----
  for(i in seq_along(locus_names)) {

    if (ploidy == 1) {
      repo[[i]]$data <- df[c(1, i+1)]
      names(repo[[i]]$data) <- c("id", "A1")
    } else {
      repo[[i]]$data <- getData(df, i, ploidy)
    }

    repo[[i]]$tidy <- tidy(repo[[i]]$data)
  }

  return(repo)
}
