#' sat_binStats
#'
#' sat_binStats assigns x values to bins and produces a tibble of useful statisitcs for each allele
#'
#'@param x raw fragment lengths
#'@param z all possible fragment lengths
#'@param y approximation of "closeness" of each z value to all x values
#'@param range the number of consecutively riseing / falling y values to take into account when finding bin limits
#'
#'@return a tibble containing basic statistics about each allele.
#'
#'@export

# function ----------------------------------------------------------------------------------------------

sat_binStats <- function (x, z, y, range = 20) {

  # Calculate the bin limits and central value of each bin ----
  limits <- sat_findLimits(z, y, range)
  center <- sat_findCenter(z, y, limits)

  # Assembal a tibble containing: ----
  binStats <- tibble::tibble(

    # Bin number; ----
    bin = seq_along(center),

    # Central value; ----
    center = center,

    # Upper and lower bin limits; ----
    lim.lower = limits[-length(limits)],
    lim.upper = limits[-1],

    # Allele Frequency; ----
    freq = sat_findStat(x, limits, length),

    # Standard deviation; ----
    sd = sat_findStat(x, limits, sd),

    # Range; ----
    range = sat_findStat(x, limits, function(x) {max(x) - min(x)}),

    # Mean ----
    mean = sat_findStat(x, limits, mean),

    # Balance ----
    balance = (mean - center)^2,

    # Kurtosis ----
    kurtosis = sat_findStat(x, limits, moments::kurtosis) - 3

  )

  return(binStats)

}
