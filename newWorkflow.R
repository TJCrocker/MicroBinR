
kde <- density(frag, n = (max(frag) - min(frag)) * 100, bw = h)$y

score <- function(frag, h) {

    K <- function(u, e = 2.71828182845905) {1 / (sqrt(2 * pi)) * e^ (-0.5 * (u^2))}


  # Compute z and y then assemble into tibble ----
  sco <- tibble::tibble(
    x = seq(min(frag) - 1, max(frag) + 1, by = 0.01),
    y = purrr::map_dbl(seq_along(x), ~{

      u <- (x[.] - frag) / h

      return(sum(K(u)))

    })
  )

  return(sco)
}

score(frag, h)

#--------------------------------------------------------------------------------------------

findLim <- function (x, y, range = 1L) {

  l <- length(x)

  eps <- .Machine$double.eps * l

  results <- list(min(x))
  why <- list(NA)
  why_up <- list(NA)
  why_down <- list(NA)

  i <- 1L + range

  repeat{

    if (all(
      !y[i - range] < y[i] | abs(y[i - range] - y[i]) < eps,
      y[i + range] > y[i] & abs(y[i + range] - y[i]) > eps
    )) {
      results <- list(results, x[i])
      why <- list(why, y[i])
      why_up <- list(why_up, y[i+1])
      why_down <- list(why_down, y[i-1])
    }

    if(i == (l-range)) {
      results <- list(results, x[i])
      why <- list(why, y[i])
      why_up <- list(why_up, NA)
      why_down <- list(why_down, y[i-1])

      break
    } else {
      i <- i+1L
    }
  }

  return(cbind(unlist(results), unlist(why_down), unlist(why), unlist(why_up)))

}

lim <- findLim(kde$x, kde$y, 1)[,1]


# ----------------------------------------------------------------------------

findCenter <- function (x, y, lim) {

  n <- 1:(length(lim) - 1)

  center <- purrr::map_dbl(n, ~{

    index <- x > lim[.] & x < lim[.+1]
    center <- x[index][y[index] == max(y[index])][[1]]

    return(center)

  })

  return(center)
}

findCenter(x, y, lim)
center <- findCenter(kde$x, kde$y, lim)

findStat(frag, lim, length)

# ------------------------------------------------------------------------------

findStat <- function (frag, lim, FUN) {

  stopifnot(is.function(FUN))

  n <- 1:(length(lim) - 1)

  stat <- purrr::map_dbl(n, ~FUN(frag[frag < lim[.+1] & frag > lim[.]]))

  return(stat)

}

avg <- findStat(frag, lim, mean)

# -----------------------------------------------------------------------------

RMSE <- function (frag, lim, center) {

  n <- 1:(length(lim) - 1)

ind_RMSE <- purrr::map_dbl(n, ~sqrt(mean((frag[frag < lim[.+1] & frag > lim[.]] - center[.])^2)))

return(sum(ind_RMSE))

}

RMSE1 <-  function (frag, lim, center) {

  n <- 1:(length(lim) - 1)

  ind_RMSE <- purrr::map(n, ~(frag[frag < lim[.+1] & frag > lim[.]] - center[.])^2)

  RMSE <- sqrt(mean(unlist(ind_RMSE)))

  return(RMSE)

}

SSE <- function (frag, lim, center) {

  n <- 1:(length(lim) - 1)

  ind_SE <- purrr::map(n, ~(frag[frag < lim[.+1] & frag > lim[.]] - center[.])^2)

  SSE <- sum(unlist(ind_SE))

  return(SSE)

}

MvMo <- function (frag, lim, center) {

  n <- 1:(length(lim) - 1)

  avg <- findStat(frag, lim, mean)

  return(sum((avg - center)^2))
}

MAXLL <- function (x, y) {

  n <- seq_along(x[-1])

  hyp <- purrr::map_dbl(n, ~sqrt(abs((y[.] - y[.+1])^2) + 1e-04))

  return(sum(hyp))

}

Area <- function (x, y) {

  return(sum(y))

}



# -------------------------------------------------------------------------------

test <- function(h, frag, FUN) {

  kde <- density(frag, n = (max(frag) - min(frag)) / 0.01, bw = h)

  lim <- findLim(kde$x, kde$y)[,1]

  center <- findCenter(kde$x, kde$y, lim)

  #avg <- findStat(frag, lim, mean)

  return(FUN(frag, lim, center))

}

eps <- test(h, frag)

test_ll <- function(h, frag, FUN) {

  kde <- score(frag, h = h)

  return(MAXLL(kde$x, kde$y))

}



test_ll(h, frag, FUN = Area)

# ---------------------------------------------------------------------------------

getbw <- function(frag) {

  bw <- optim(par = 0.4, fn = test, frag = frag, method = "Brent", lower = 0.001, upper = 0.15)

  return(bw)

}

# --------------------------------------------------------------------------------

bin.locus <- function (frag) {

  bw <- getbw(frag)
  kde <-  density(frag, n = (max(frag) - min(frag)) * 100, bw = bw$par)

  lim <-findLim(kde$x, kde$y)[,1]
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

Tc6 <- bin.locus(frag)

# -----------------------------------------------------------------------------------

get.binned <- function(cluster) {

}

# -----------------------------------------------------------------------------------

x <-  Tc6$kde$x
y <- Tc6$kde$y
center <- Tc6$stat$center
lim <- Tc6$stat$lim
title <- "a title"

plot <- function (frag, x, y, center, lim, title = NULL) {


  df <- tibble::tibble(x = x,
                       y = y,
                       bin = purrr::map_dbl(x, ~sum(dplyr::cumall(. > lim[-1])) + 1)
                       )

  df1 <- tibble::tibble(frag = frag,
                        y = -max(y) / 10,
                        yend = 0
                        )

  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = as.factor(bin%%2))) +
    ggplot2::geom_path(group = 1, size = 1.5) +
    ggplot2::scale_x_continuous(breaks = center) +
    ggplot2::geom_segment(data = df1, inherit.aes = FALSE, ggplot2::aes(x = frag, y = y, xend = frag, yend = yend)) +
    ggplot2::labs(x = "Fragment Length", y = "Density", title = title) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -25),
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    )

  return(plot)
}


i <- 3

frag <- repo[[i]]$tidy$frag_len[!repo[[i]]$tidy$duplicate]

Tc6 <- bin.locus(frag)

plot(Tc6$frag, Tc6$kde$x, Tc6$kde$y, Tc6$stat$center, Tc6$stat$lim, stringr::str_c("h = ", Tc6$bw$par, ", error = ", Tc6$bw$value, ", locus = ", i))

# ------------------------------------------------------------------------------------------------

h1 <- seq(0.01, 10, 0.01)

error <- tibble::tibble(
  h = h1,
  Area = vector("numeric", length(h1))
)

for (i in seq_along(h1)) {

  error$Area[i] <- test_ll(h1[i], frag, FUN = Area)
  print(i)

}

error %>%
  mutate(
    Area = Area / max(Area)
  ) %>%
  tidyr::gather(key = "fun", value = "error", -h) %>%
  ggplot(aes(h, error)) +
  geom_point() +
  facet_wrap(~fun)



