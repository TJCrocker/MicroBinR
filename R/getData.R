#' sat_getData
#'
#' sat_getData returns the ID and fragment length columns for a locus from rectangular input data
#'
#'@param data a dataframe like object where the first column is a unique identifier for each genotype and subseqent columns contain raw fragment lengths found for that locus.
#'
#'@return
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_getData <- function (df, locus_num, ploidy) {

  # return columns from df containing ID and frag_len for a locus ----
  data <- df[c(1, locus_num * (ploidy) + (0:(ploidy-1)))]
  names(data) <- c("id", paste(rep("A", ploidy), as.character(1:ploidy), sep =""))

  return(data)

}
