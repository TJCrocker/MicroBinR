#' sat_assemble
#'
#' sat_getData returns the ID and fragment length columns for a locus from rectangular input data
#'
#'@param data a dataframe like object where the first column is a unique identifier for each genotype and subseqent columns contain raw fragment lengths found for that locus.
#'
#'@return
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_return <- function (repo, stage) {


  # Find ploidy, number of loci and number of genotypes ----
      p <- length(repo[[1]][[stage]][-1])
      l <- length(repo)
      L <- length(repo[[1]][[stage]][[1]])


      # Build output object ----
      out <- tibble::as_tibble(matrix(NA, nrow = L, ncol = 1 + (l * p)))
      names_out <- vector("character", (l*p) + 1)

      # Populate ID column ----
      out[1] <- repo[[1]][[stage]][[1]]
      names_out[1] <- "id"

      # Iteratively populate ouput ----
      for (i in 1:l) {

        out[,(((i - 1) * p) + 1):(i * p) + 1] <- repo[[i]][[stage]][-1]

        names_out[(((i - 1) * p) + 1):(i * p) + 1] <- c(names(repo)[i], stringr::str_c(names(repo[i]), "_", 1:(p-1)))

      }

      # Name output ----
      names(out) <- names_out

      return(out)
    }
