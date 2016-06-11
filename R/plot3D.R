#' Surface plot of two-dimensional test function.
#'
#' @param x [\code{smoof_function}]\cr
#'   Two-dimensional snoof function.
#' @param length.out [\code{integer(1)}]\cr
#'   Determines the \dQuote{smoothness} of the grid. The higher the value, the
#'   smoother the function landscape looks like. However, you should avoid setting
#'   this parameter to high, since with the \code{contour} option set to \code{TRUE}
#'   the drawing can take quite a lot of time. Default is \code{100}.
#' @param package [\code{character(1)}]\cr
#'   String describing the package to use for 3D visualization.
#'   At the moment \dQuote{plot3D} (package \pkg{plot3D}) and
#'   \dQuote{plotly} (package \pkg{plotly}) are supported.
#'   The latter opens a highly interactive plot in a web brower
#'   and is thus suited well to explore a function by hand.
#'   Default is \dQuote{plot3D}.
#' @param ... [any]\cr
#'    Furhter parameters passed to method used for visualization
#'    (which is determined by the \code{package} argument.
#' @examples
#' library(plot3D)
#' fn = makeRastriginFunction(dimensions = 2L)
#' \dontrun{
#' # use the plot3D::persp3D method (default behaviour)
#' plot3D(fn)
#' plot3D(fn, contour = TRUE)
#' plot3D(fn, image = TRUE, phi = 30)
#'
#' # use plotly::plot_ly for interactive plot
#' plot3D(fn, method = "plotly")
#' }
#' @export
plot3D = function(x, length.out = 100L, package = "plot3D", ...) {
  assertClass(x, "smoof_function")
  assertInt(length.out, lower = 10L)
  assertChoice(package, choices = c("plot3D", "plotly"))

  obj.fn = x
  n = getNumberOfParameters(obj.fn)
  par.set = getParamSet(obj.fn)
  if (n != 2L) {
    stopf("Surface plots are possible only for 2D numeric functions, but your function expects %i parameters.", n)
  }
  if (!isNumeric(par.set, include.int = FALSE)) {
    stopf("Surface plots are possible only for 2D numeric functions, but your function expects non-numeric parameters.")
  }

  # build z[i, j] = f(x[i], y[j]) matrix
  lower = getLower(par.set)
  upper = getUpper(par.set)
  x = seq(lower[1], upper[1], length.out = length.out)
  y = seq(lower[2], upper[2], length.out = length.out)
  grid = expand.grid(x, y)
  z = apply(grid, 1, obj.fn)
  dim(z) = c(length.out, length.out)

  # load package whose 3D plot is used
  requirePackages(package, why = "smoof.plot3D")
  if (package == "plot3D") {
    return(plot3D::persp3D(z = z, x = x, y = y, ...))
  } else if (package == "plotly") {
    return(plotly::plot_ly(z = z, type = "surface", ...))
  }
}
