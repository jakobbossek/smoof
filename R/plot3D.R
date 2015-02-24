#' Surface plot of two-dimensional test function.
#'
#' @param x [\code{snoof_function}]\cr
#'   Two-dimensional snoof function.
#' @param length.out [\code{integer(1)}]\cr
#'   Determines the \dQuote{smoothness} of the grid. The higher the value, the
#'   smoother the function landscape looks like. However, you should avoid setting
#'   this parameter to high, since with the \code{contour} option set to \code{TRUE}
#'   the drawing can take quite a lot of time. Default is \code{100}.
#' @param ... [any]\cr
#'    Furhter parameters passed to \code{\link[plot3D]{persp3D}}.
#' @export
plot3D = function(x, length.out = 100L, ...) {
    assertClass(x, "smoof_function")
    assertInt(length.out, lower = 10L, na.ok = FALSE)
    obj.fn = x
    n = getNumberOfParameters(obj.fn)
    if (n != 2L) {
        stopf("Surface plots are possible only for two-dimensional functions, but your function expects %i parameters", n)
    }
    par.set = getParamSet(obj.fn)
    lower = getLower(par.set)
    upper = getUpper(par.set)
    x = seq(lower[1], upper[1], length.out = length.out)
    y = seq(lower[2], upper[2], length.out = length.out)
    grid = expand.grid(x, y)
    z = apply(grid, 1, obj.fn)
    dim(z) = c(length.out, length.out)
    requirePackages("plot3D", why = "smoof.plot3D")
    return(plot3D::persp3D(z = z, x = x, y = y, ...))
}