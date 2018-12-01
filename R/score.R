#' score
#'
#' Calculates a kernel denstiy estimation without wighting the ouput by n or bandwidth. The resulting distribution is calculated at a uniform reolution of 0.01 fwith fixed tails of 1 - min(x) to 1 + max(x).
#'
#'@param frag a vector of fragment lengths
#'@param h bandwidth
#'
#'@return x and y values for an unweighted disribution
#'
#'@export

score <- function(frag, h) {

  K <- function(u, e = 2.71828182845905) {1 / (sqrt(2 * pi)) * e^ (-0.5 * (u^2))}


  # Compute x and y then assemble into tibble ----
  sco <- tibble::tibble(
    x = seq(min(frag) - 1, max(frag) + 1, by = 0.01),
    y = purrr::map_dbl(seq_along(x), ~{

      u <- (x[.] - frag) / h

      return(sum(K(u)))

    })
  )

  return(sco)
}
