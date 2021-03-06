#' Scatter plot with images at the specified points
#'
#' Create a scatter plot of image data. Images are plotted in random order, and
#' are only plotted if they do not overlap with an already-plotted image. See
#' the examples section for an example using MNIST data.
#'
#' @param latent An n-by-2 matrix specifying where points in observed should be
#'   plotted.
#' @param observed An n-by-q matrix of image data to be plotted at the
#'   corresponding points in \code{latent}.
#' @param observed.dim A numeric vector of length 2 giving the image dimensions
#'   for each row in \code{observed}. \code{prod(observed.dim) ==
#'   ncol(observed)} must be true.
#' @param bins The number of colors to generate from col.palette.
#' @param col.palette A color palette function, such as rainbow or heat.colors,
#'   or a vector of colors of length \code{bins}. Default is
#'   \link[viridis]{viridis}.
#' @param image.density How dense should the plotted images be. Higher
#'   \code{image.density} means smaller images.
#' @param num.attempts The number of non-overlapping plots to generate. The one
#'   which plots the most points will be chosen for the output plot.
#' @param ... Any additional parameters to be passed to \link{plot}.
#' @param interpolate Whether to interpolate the plotted images (see
#'   \link{rasterImage} for details)
#' @param border.colors Colors for borders to be drawn around points, for
#'   example to show class membership. If supplied, a colored border is drawn
#'   around each plotted image. If a vector of integers is supplied, colors are
#'   drawn from a colorblind-friendly palette (max 8 colors).
#' @param border.width Line width for the borders, relative to default.
#'
#' @return A logical vector indicating which rows have been plotted (returned
#'   invisibly).
#' @export
#'
#' @examples
#' #Plot PCA of the MNIST data
#' mnist <- ImageScatterPlot::mnist
#' latent <- prcomp(mnist)$x[,1:2]
#' ImageScatterPlot(latent,
#'                  mnist,
#'                  rep(28,2),
#'                  col.palette = grey.colors(256, start=1, end=0),
#'                  num.attempts = 10,
#'                  image.density=15,
#'                  interpolate=T,
#'                  main="First two principal components of MNIST handwritten digits")
ImageScatterPlot <- function(latent,
                             observed,
                             observed.dim,
                             bins=256,
                             col.palette=viridis::viridis,
                             image.density=10,
                             num.attempts=100,
                             interpolate=F,
                             border.colors=NULL,
                             border.width= par("lwd"),
                             ...) {
  stopifnot(prod(observed.dim) == ncol(observed))
  stopifnot(nrow(latent) == nrow(observed))
  if (is.function(col.palette)) col.palette <- col.palette(bins)
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  if (is.numeric(border.colors)) {
    stopifnot(max(border.colors) <= 8 & min(border.colors) >= 1 & all(border.colors %% 1 == 0))
    border.colors <- cbbPalette[border.colors]
  }
  observed.cut <- matrix(cut(observed, bins, labels = F), nrow(observed))
  plot(latent, type="n", ...)
  xwidth <- (max(latent[, 1]) - min(latent[, 1])) / image.density
  ywidth <- (max(latent[, 2]) - min(latent[, 2])) / image.density
  max.plotted <- logical(nrow(latent))
  for (i in 1:num.attempts) {
    plotted <- logical(nrow(latent))
    for (i in sample(nrow(latent))) {
      plotted.dist <- abs(t(t(latent[plotted, , drop=F]) - latent[i, ]))
      if (!any(plotted.dist[, 1] < xwidth & plotted.dist[, 2] < ywidth)) {
        plotted[i] <- TRUE
      }
    }
    if (sum(plotted) > sum(max.plotted)) {
      max.plotted <- plotted
    }
  }

  for (i in 1:nrow(latent)) {
    if (max.plotted[i]) {
      if (!is.null(border.colors)) {
        im.xwidth <- xwidth * (1 - 0.1 * min(border.width, 9))
        im.ywidth <- ywidth * (1 - 0.1 * min(border.width, 9))
        xleft <- latent[i, 1] - xwidth/2
        xright <- xleft + xwidth
        ybottom <- latent[i, 2] - ywidth/2
        ytop <- ybottom + ywidth
        rasterImage(border.colors[i], xleft, ybottom, xright, ytop)
      } else {
        im.xwidth <- xwidth
        im.ywidth <- ywidth
      }
      xleft <- latent[i, 1] - im.xwidth/2
      xright <- xleft + im.xwidth
      ybottom <- latent[i, 2] - im.ywidth/2
      ytop <- ybottom + im.ywidth
      raster.matrix <- matrix(col.palette[observed.cut[i, ] ], nrow=observed.dim[1])
      rasterImage(raster.matrix, xleft, ybottom, xright, ytop, interpolate = interpolate)
    }
  }
  invisible(max.plotted)
}

