#' bin.locus
#'
#' Bins microsatilite fragment lengths useing a clustering operation based on kernel density estimation.
#'
#'@param frag a vector of microsatilite fragment lengths
#'
#'@return a list containing results of bandwidth estimation, KDE, bin limits, central values, mean and frequency.
#'
#'@export
#'

bin.locus <- function (frag) {

  bw <- getbw(frag)
  kde <-  density(frag, n = (max(frag) - min(frag)) * 100, bw = bw$par)

  lim <-findLim(kde$x, kde$y)
  center <-  findCenter(kde$x, kde$y, lim)
  avg <- findStat(frag, lim, mean)
  freq <- findStat(frag, lim, length)


  results <-  list(
    frag = frag,
    bw = bw,
    kde = kde,
    stat = list(
      lim = lim,
      center = center,
      avg = avg,
      freq = freq)
  )

  return(results)

}
