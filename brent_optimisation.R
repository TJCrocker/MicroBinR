repo <- MicroBinR::sat_makeDatabase(MicroBinR::tilia, 2)
repo <- MicroBinR::sat_assess(repo)

repo

x <- repo$Tc6$tidy$frag_len[!repo$Tc6$tidy$duplicate]
z <- repo$Tc6$scor$z
y <- repo$Tc6$scor$y
center <- repo$Tc6$stat$center
lim.upper <- repo$Tc6$stat$lim.upper
title <- stringr::str_c("Bandwidth = ", as.character(repo$Tc6$info$sgma))

sat_plot(x, z, y, center, lim.upper, title)

purrr::map(names(repo), ~sum(repo[[.]]$stat$Skewness) * length(repo[[.]]$stat$Skewness))

repo$Tc6$stat

test <- function (h, x ) {

  kde <- density(x, n = (max(x) - min(x)) * 100, bw = h)

  stat <- sat_binStats(x = x, z = kde$x, y = kde$y)

  error <- sat_min(e = stat$balance)

  return(error)

}

test(h = 0.4, x = x)

  sat_assess1 <- function (repo, h= NULL, range = NULL, K = NULL) {

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

      # find optimal bandwidth ----

      minimised <- optim(par = 0.4, fn = test, x = x, method = "Brent", lower = 0.2, upper = 0.8)
      error <- minimised$value
      h <- minimised$par

      # calculate and store z and y ----
      repo[[i]]$scor <- sat_score(x = x, h = h, K = K)

      z <- repo[[i]]$scor$z
      y <- repo[[i]]$scor$y

      # calculate and store bin stats ----
      repo[[i]]$stat <- sat_binStats(x, z, y, range[[i]])

      center <- repo[[i]]$stat$center
      lim.upper <- repo[[i]]$stat$lim.upper

      # calculate and store plot ----
      repo[[i]]$plot <- sat_plot(x, z, y, center, lim.upper, title = stringr::str_c(names(repo)[[i]], ": bandwidth = ", as.character(h), ", error = ", as.character(error)))

      # update completion status ----
      repo[[i]]$info$complete <- TRUE
    }

    return(repo)
  }

  repo <- MicroBinR::sat_makeDatabase(readr::read_csv("Tilia.csv"), 2)
  repo <- MicroBinR::sat_makeDatabase(MicroBinR::tilia, 2)
  print(repo_e_n_kurt$Tc915) <- sat_assess1(repo)
  repo_e_n <-
  print(repo_e_kurt$Tc915) <- repo_e_n_kurt
  repo_n_kurt
  repo_n
  repo_kurt
  repo_e <- sat_assess1(repo[length(repo)])

 print(repo)

  length(names(repo))


  (repo$Tc943$stat$center[2] - repo$Tc943$stat$mean[2])^2

  sat_findLimits(kde$x, kde$y, range = 1)

  #-------------------------------------------------------------------------------------------------------------

test <- function(h, frag) {

  kde <- density(frag, n = (max(frag) - min(frag)) * 100, bw = h)

  stat <- sat_binStats(x = frag, z = kde$x, y = kde$y)

  error <- sat_min(e = stat$balance)

  return(error)

}

  frag <- repo$Tc6$tidy$frag_len[!repo$Tc6$tidy$duplicate]
sat_getbw <- function(frag) {

  bw <- optim(par = 0.4, fn = test, frag = frag, method = "Brent", lower = 0.2, upper = 0.8)

}

bw <- sat_getbw(frag)

 kde <- density(frag, n = (max(frag) - min(frag)) * 100, bw = bw$par)

 x <- kde$x
 y <- kde$y

 stat <- sat_binStats(x = x, z = kde$x, y = kde$y, 2)

 title <- stringr::str_c("bandwidth = ", bw$par, " error = ", bw$value)

 sat_plot(x, kde$x, kde$y, center = stat$center, lim.upper = stat$lim.upper, title = title)

