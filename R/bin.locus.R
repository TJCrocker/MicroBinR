#' bin.locus
#'
#' Bins microsatilite fragment lengths useing a clustering operation based on kernel density estimation.
#'
#'@param frag a vector of microsatilite fragment lengths
#'@param p the cut-off p-value for selecting errors
#'
#'@return a list containing results of bandwidth estimation, KDE, bin limits, central values, mean and frequency.
#'
#'@export
#'

bin.locus <- function (frag, p = 0.05) {

  frag <- sort(frag)

  bw <- getbw(frag)
  kde <-  density(frag, n = (max(frag) - min(frag)) * 100, bw = bw$par)

  lim <-findLim(kde$x, kde$y)
  center <-  findCenter(kde$x, kde$y, lim)
  avg <- findStat(frag, lim, mean)
  freq <- findStat(frag, lim, length)

  rsid<- findVal(frag, lim, function(x, center) {return(center - x)}, center = center)
  prob <- pnorm(q = rsid, mean = mean(rsid), sd = sd(rsid))
  redo <- is.na(prob) | prob < (p / 2) | prob > (1- (p / 2))

  results <-  list(
    frag = frag,
    p = prob,
    rediduals = rsid,
    redo = redo,
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
