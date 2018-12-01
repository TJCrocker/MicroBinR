#' ggDensityPlot
#'
#' produces a density plot of fragment lengths alternately colour coded by bin.
#'
#'@param frag a vector of microsatilite fragment length
#'@param x the x component of the density plot
#'@param y the y component of the density plot
#'@param center a vector of central values of each bin
#'@param lim a vector of bin limits
#'@param title a title
#'
#'@return a list containing results of bandwidth estimation, KDE, bin limits, central values, mean and frequency.
#'
#'@export
#'


ggDensityPlot <- function (frag, x, y, center, lim, title = NULL) {


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
    ggplot2::coord_fixed(ratio = (max(x) - min(x)) / (max(y) * 2)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -25),
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    )

  return(plot)
}
