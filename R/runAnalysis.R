#' sat_assess
#'
#' runs sat_score, sat_binStats and sat_plot for all loci for which info$complete = "FALSE". The product of each function is saved in a data repository, upon completion the complete placeholder is changes to "TRUE"
#'
#'@param repo a database containing data for each loci (produced by sat_makeDatabase)
#'@param sgma a vector containing sigma values for each loci passed to sat_score (if null defaults to 0.4)
#'@param range a vector containing ranges for each loci passed to sat_binStats if (null defaults to 20)
#'
#'
#'@return a database updated with the ouputs of sat_score, sat_binStats and sat_plot
#'
#'@export
#'
# Function ---------------------------------------------------------------------------------------------------------------

sat_runAnalysis <- function(df, ploidy, sgma = NULL, range = NULL) {

  repo <- sat_makeDatabase(df, ploidy)
  repo <- sat_assess(repo, sgma, range)

  return(repo)
}
