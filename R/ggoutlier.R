# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(ggplot2)

StatOutlier <- ggproto(
  "StatOutlier", Stat,
  compute_group = function(data, scales) {
    # get the limits
    x_min <- data$xmin[1]
    x_max <- data$xmax[1]
    y_min <- data$ymin[1]
    y_max <- data$ymax[1]
    # some offset of the outliers
    x_offset <-
      (min(x_max, scales$x$range$range[2]) -
       min(x_min, scales$x$range$range[1])) * .03
    y_offset <-
      (min(y_max, scales$y$range$range[2]) -
       min(y_min, scales$y$range$range[1])) * .03
    # move outliers to the limits
    data$x[data$x < x_min] <- x_min - x_offset
    data$x[data$x > x_max] <- x_max + x_offset
    data$y[data$y < y_min] <- y_min - y_offset
    data$y[data$y > y_max] <- y_max + y_offset
    data
  },
  required_aes = c("x", "y")
)

stat_outlier <- function(mapping = NULL, data = NULL, geom = "PointOutlier",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  layer(
    stat = StatOutlier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomPointOutlier <- ggproto(
  "GeomPointOutlier", GeomPoint,
  draw_panel = function(data, panel_params, coord, ...) {
    # get the limits
    x_min <- data$xmin[1]
    x_max <- data$xmax[1]
    y_min <- data$ymin[1]
    y_max <- data$ymax[1]
    # turn values at the limits into triangles
    data$shape[data$x<x_min] <- 23
    data$shape[data$x>x_max] <- 23
    data$shape[data$y<y_min] <- 25
    data$shape[data$y>y_max] <- 24
    GeomPoint$draw_panel(data, panel_params, coord, ...)
  }
)

geom_point_outlier <- function(
  stat = StatOutlier, position = "identity", ...) {
    layer(geom = GeomPointOutlier, stat = stat, position = position, ...)
}
