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

sat_assess <- function (repo, h = NULL, range = NULL, K = NULL) {

  # if no sigma / range return to default values ----
  if (is.null(h)) {
    h <- rep(0.4, length(repo))
  }

  if (is.null(range)) {
    range <- rep(20, length(repo))
  }

  # If h or range has changed completion status is changed to FALSE ----
  for (i in seq_along(repo)) {
    if (any(h[[i]] != repo[[i]]$info$sgma, range[[i]] != repo[[i]]$info$range)) {
      repo[[i]]$info$complete <- FALSE
    }
  }

  # retrieve completion status of loci ----
  comp <- purrr::map_lgl(seq_along(repo), ~repo[[.]]$info$complete)
  #nms <- names(repo)[!comp]
  sqen <- seq_along(repo)[!comp]

  # for each incomplete loci ----
  for (i in sqen) {

    # retrive x ----
    x <- repo[[i]]$tidy$frag_len[!repo[[i]]$tidy$duplicate]

    # calculate and store z and y ----
    repo[[i]]$scor <- sat_score(x = x, h = h[[i]], K = K)

    z <- repo[[i]]$scor$z
    y <- repo[[i]]$scor$y

    # calculate and store bin stats ----
    repo[[i]]$stat <- sat_binStats(x, z, y, range[[i]])

    center <- repo[[i]]$stat$center
    lim.upper <- repo[[i]]$stat$lim.upper

    # calculate and store plot ----
    repo[[i]]$plot <- sat_plot(x, z, y, center, lim.upper, title = stringr::str_c("h = ", as.character(h[[i]])))

    # update completion status ----
    repo[[i]]$info$complete <- TRUE
  }

  return(repo)
}


sat_assess <- function (repo, h = NULL, range = NULL, K = NULL) {

  # if no sigma / range return to default values ----
  if (!is.null(h)) {
    stopifnot(length(h) != length(repo))
  }

  if (!is.null(range)) {
    stopifnot(length(range) != length(repo))
  } else {
    range <- rep(2, length(repo))
  }

  # If h or range has changed completion status is changed to FALSE ----
  for (i in seq_along(repo)) {
    if (any(h[[i]] != repo[[i]]$info$h, range[[i]] != repo[[i]]$info$range)) {
      repo[[i]]$info$complete <- FALSE
    }
  }

  # retrieve completion status of loci ----
  comp <- purrr::map_lgl(seq_along(repo), ~repo[[.]]$info$complete)
  #nms <- names(repo)[!comp]
  sqen <- seq_along(repo)[!comp]

  # for each incomplete loci ----
  for (i in sqen) {

    # retrive x ----
    frag <- repo[[i]]$tidy$frag_len[!repo[[i]]$tidy$duplicate]

    # get h ----

    if(is.null(repo[[i]]$info$h)) {
      h <- sat_getbw(frag)$par
    }

    # calculate and store z and y ----
    repo[[i]]$kde <- density(frag, n = (max(frag) - min(frag)) * 100, bw = h)

    x <- repo[[i]]$kde$x
    y <- repo[[i]]$kde$y

    # calculate and store bin stats ----
    repo[[i]]$stat <- sat_binStats(frag, x, y, range[[i]])

    center <- repo[[i]]$stat$center
    lim.upper <- repo[[i]]$stat$lim.upper

    # calculate and store plot ----
    repo[[i]]$plot <- sat_plot(frag, x, y, center, lim.upper)

    # update completion status ----
    repo[[i]]$info$complete <- TRUE
  }

  return(repo)
}
