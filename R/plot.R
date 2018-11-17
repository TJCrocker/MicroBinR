#' sat_plot
#'
#' sat_plot plots all possible fragment lengths (z) against the "closness" of all possible fragment lengths to observed raw fragment lengths (y)
#'
#'@param z a vector of possible fragment lengths
#'@param y a vector of desity of fragment lengths, must be same length as z
#'@param center a vector representing the center of each bin
#'@param lim.upper a vector representing the upper limit of each bin
#'
#'
#'@return
#'
#'@export
#'
# function --------------------------------------------------------------------------------------------------------------------

sat_plot <- function (x, z, y, center, lim.upper, title = NULL) {


  df <- tibble::tibble(z = z,
                       y = y,
                       bin = purrr::map_dbl(z, function(z) {
                         sum(dplyr::cumall(z > lim.upper)) + 1
                       }))

  df1 <- tibble::tibble(x = x,
                y = -max(y) / 10,
                yend = 0,
                bin = purrr::map_dbl(x, function(x) {
                  sum(dplyr::cumall(x > lim.upper)) + 1
                }))

  plot <- ggplot2::ggplot(df, ggplot2::aes(z, y, colour = as.factor(bin%%2))) +
    ggplot2::geom_path(group = 1, size = 1.5) +
    ggplot2::scale_x_continuous(breaks = center) +
    ggplot2::geom_segment(data = df1, inherit.aes = FALSE, ggplot2::aes(x = x, y = y, xend = x, yend = yend, colour = as.factor(bin%%2)), palette) +
    scale_color_grey() +
    ggplot2::labs(x = "Fragment Length", y = "Density", title = title) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -25),
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
      )

  return(plot)
}
