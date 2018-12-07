#' findProb
#'
#' retuns the probability of observing each fragment given the other fragments in that bin assuming normal distribution.
#'
#'@param frag a vector of fragment lengths
#'@param lim a vector of bin limits
#'
#'@return p value of each fragment. NA for unique bins.
#'
#'@export

findProb <- function(frag, lim) {

  n <- seq_along(lim[-1])

  ord <- seq_along(frag)

  result <- purrr::map(n, ~{

    index <- frag > lim[.] & frag < lim[.+1]
    avg <- mean(frag[index])
    s <- sd(frag[index])

    quant <- abs(abs(avg - frag[index]) - avg)

    prob <- pnorm(q = quant, mean = avg, sd = s)

    names(prob) <- ord[index]

    return(unlist(prob))
  })

  return(unlist(result))
}
