#' Generate \code{\link[ggplot2]{ggplot}} object.
#'
#' @param x [\code{smoof_function}]\cr
#'   Objective function.
#' @param show.optimum [\code{logical(1)}]\cr
#'   If the function has a known global optimum, should its location be
#'   plotted by a point or multiple points in case of multiple global optima?
#'   Default is \code{FALSE}.
#' @param main [\code{character(1L)}]\cr
#'   Plot title.
#'   Default is the name of the smoof function.
#' @param render.levels [\code{logical(1)}]\cr
#'   For 2D numeric functions only: Should an image map be plotted? Default is
#'   \code{FALSE}.
#' @param render.contours [\code{logical(1)}]\cr
#'   For 2D numeric functions only: Should contour lines be plotted? Default is
#'   \code{TRUE}.
#' @param use.facets [\code{logical(1)}]\cr
#'   For mixed functions only: Should the plot be splitted by the discrete values
#'   or should the different values be distinguished by colour in a single plot?
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @examples
#' library(ggplot2)
#' fn = makeHimmelblauFunction()
#' print(autoplot(fn))
#' print(autoplot(fn, render.levels = TRUE, render.contours = FALSE))
#' print(autoplot(fn, show.optimum = TRUE))
#' @export
autoplot.smoof_function = function(x,
  show.optimum = FALSE,
  main = getName(x),
  render.levels = FALSE, render.contours = TRUE,
  use.facets = FALSE,
  ...) {
  checkPlotFunParams(x)

  assertFlag(show.optimum, na.ok = FALSE)
  assertString(main, na.ok = TRUE)
  assertFlag(render.levels, na.ok = FALSE)
  assertFlag(render.contours, na.ok = FALSE)
  assertFlag(use.facets, na.ok = FALSE)

  par.set = getParamSet(x)
  par.types = getParamTypes(par.set)
  par.types.count = getParamTypeCounts(par.set)
  pars = par.set$pars
  par.names = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  n.pars = length(pars)

  # determine IDs of numeric and factor-like parameters
  par.types = getParamTypes(par.set, df.cols = TRUE, with.nr = TRUE)
  numeric.idx = which(par.types == "numeric")
  discrete.idx = which(par.types %in% c("factor", "logical"))

  # how many numeric/discrete parameters do exist?
  n.numeric = length(numeric.idx)
  n.discrete = length(discrete.idx)

  if (n.pars > 4L) {
    stopf("At most 4D funtions with mixed parameter spaces can be visualized.")
  }

  if (par.types.count$numeric > 2 || (par.types.count$discrete + par.types.count$logical) > 2L) {
    stopf("Not possible to plot this combination of parameters.")
  }

  if (n.numeric > 1L && !(render.levels || render.contours)) {
    stopf("For functions with 2 numeric parameters one of render.levels or render.contours needs to be TRUE.")
  }

  grid = generateDataframeForGGPlot2(x)

  if (n.numeric == 1L) {
    pl = ggplot(grid, aes_string(x = par.names[numeric.idx], y = "y")) + geom_line()
  }
  if (n.numeric == 2L) {
    pl = ggplot(grid, aes_string(x = par.names[numeric.idx[1L]], y = par.names[numeric.idx[2L]]))
    if (render.levels) {
      # nice color palette for render.levels
      # see http://learnr.wordpress.com/2009/07/20/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-6/
      brewer.div = colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")

      pl = pl + geom_tile(aes_string(fill = "y"))
      pl = pl + scale_fill_gradientn(colours = brewer.div(200))
      pl = pl + theme(legend.position = "top")
    }
    if (render.contours) {
      pl = pl + stat_contour(aes_string(z = "y", fill = NULL), colour = "gray", alpha = 0.8)
    }
  }

  # split with facets if discrete values exist
  if (n.discrete > 0L) {
    if (n.discrete == 2L) {
      formula = as.formula(sprintf("%s ~ %s", par.names[discrete.idx[1L]], par.names[discrete.idx[2L]]))
    } else {
      formula = as.formula(sprintf(". ~ %s", par.names[discrete.idx]))
    }
    pl = pl + facet_grid(formula)
  }


  if (show.optimum && hasGlobalOptimum(x)) {
    glob.opt = getGlobalOptimum(x)
    glob.opt.df = glob.opt$param
    glob.opt.df[, "y"] = as.numeric(glob.opt$value)
    print(head(glob.opt.df))
    if (n.numeric == 1L) {
      pl = pl + geom_point(glob.opt.df, mapping = aes_string(x = par.names[numeric.idx[1L]], y = "y"), colour = "tomato")
    } else {
      pl = pl + geom_point(glob.opt.df, mapping = aes_string(x = par.names[numeric.idx[1L]], y = par.names[numeric.idx[2L]]), colour = "tomato")
    }
  }

  return(pl)

  stop("autoplot")

  mapping = list("1Dnumeric" = autoplot1DNumeric, "2Dnumeric" = autoplot2DNumeric, "2DMixed" = autoplot2DMixed)
  autoPlotFun = getInternalPlotFunction(x, mapping = mapping)

  autoPlotFun(x,
    show.optimum = show.optimum,
    main = main,
    render.levels = render.levels,
    render.contours = render.contours,
    use.facets = use.facets,
    ...
  )
}

#' @export
autoplot.smoof_wrapped_function = function(x,
  show.optimum = FALSE,
  main = getName(x),
  render.levels = FALSE, render.contours = TRUE,
  use.facets = FALSE,
  ...) {
  autoplot(getWrappedFunction(x), show.optimum, main,
    render.levels, render.contours,
    use.facets,
    ...
  )
}
