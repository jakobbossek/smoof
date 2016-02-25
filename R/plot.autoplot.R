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

  if (n.pars > 4L) {
    stopf("At most 4D funtions with mixed parameter spaces can be visualized.")
  }

  if (par.types.count$numeric > 2 || (par.types.count$discrete + par.types.count$logical) > 2L) {
    stopf("Not possible to plot this combination of parameters.")
  }

  grid = generateDataframeForGGPlot2(x)

  # determine IDs of numeric and factor-like parameters
  par.types = getParamTypes(par.set, df.cols = TRUE, with.nr = TRUE)
  numeric.idx = which(par.types == "numeric")
  discrete.idx = which(par.types %in% c("factor", "logical"))

  # how many numeric/discrete parameters do exist?
  n.numeric = length(numeric.idx)
  n.discrete = length(discrete.idx)

  if (n.numeric == 1L) {
    pl = ggplot(grid, aes_string(x = par.names[numeric.idx], y = "y")) + geom_line()
  }
  if (n.numeric == 2L) {
    pl = ggplot(grid, aes_string(x = par.names[numeric.idx[1L]], y = par.names[numeric.idx[2L]])) + stat_contour(aes_string(z = "y", fill = NULL), colour = "gray", alpha = 0.8)
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

generateDataframeForGGPlot2 = function(fun) {
  par.set = getParamSet(fun)
  par.types = getParamTypes(par.set)
  par.types.count = getParamTypeCounts(par.set)
  pars = par.set$pars
  par.names = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  n.pars = length(pars)

  # build data.frame
  values = lapply(seq(n.pars), function(i) {
    the.par = pars[[i]]
    par.name = names(pars)[i]
    par.type = par.types[i]
    values = NULL
    if (par.type == "numeric") {
      values = seq(the.par$lower, the.par$upper, length.out = 50L)
    } else if (par.type == "numericvector") {
      values = lapply(1:the.par$len, function(i) {
        seq(the.par$lower[i], the.par$upper[i], length.out = 50L)
      })
    } else if (par.type == "discrete") {
      values = unlist(the.par$values, use.names = FALSE)
    } else if (par.type == "logical") {
      values = as.character(unlist(the.par$values, use.names = FALSE))
    }
    return(values)
  })

  flatten = function(x) {
    if (is.list(x)) {
      Reduce(c, lapply(x, flatten))
    } else {
      list(x)
    }
  }

  values = flatten(values)

  grid = do.call(expand.grid, values)
  print("====")
  print(head(grid))
  print(colnames(grid))
  print(getParamIds(par.set, with.nr = TRUE, repeated = TRUE))
  colnames(grid) = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)

  # now compute the function values and append
  #FIXME: check if one of the parameters is named "y"
  #FIXME: apply converts each line to character!
  grid$y = NA
  par.names2 = getParamIds(par.set, with.nr = FALSE, repeated = TRUE)
  for (i in 1:nrow(grid)) {
    # transform i-th row to a named list of vectors
    # as.list is not sufficient since we need to handle vector params
    x = list()
    for (j in 1:length(par.names2)) {
      if (is.null(x[[par.names2[j]]])) {
        x[[par.names2[j]]] = grid[i, j]
      } else {
        x[[par.names2[j]]] = c(x[[par.names2[j]]], grid[i, j])
      }
    }
    grid[i, "y"] = fun(x = x)
  }
  return(grid)
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

autoplot1DNumeric = function(x, data, show.optimum, main, render.contours, render.levels, use.facets, ...) {
  # extract data
  par.set = getParamSet(x)
  par.name = getParamIds(par.set)

  # get lower and upper bounds
  lower = getBounds(bound = getLower(par.set), default = -10L)
  upper = getBounds(bound = getUpper(par.set), default = 10L)

  data = generateDataframeForGGPlot(fn = x, sequences = list(seq(lower, upper, by = 0.01)), par.set = par.set)

  # finally draw data
  pl = ggplot(data = data, mapping = aes_string(x = par.name, y = "y"))
  if (isNoisy(x)) {
    pl = pl + geom_point()
  } else {
    pl = pl + geom_line()
    if (show.optimum && hasGlobalOptimum(x)) {
      global.optimum = getGlobalOptimum(x)
      pl = pl + geom_vline(xintercept = as.numeric(global.optimum$param), linetype = "dashed", colour = "grey")
      point.data = data.frame(x = unlist(global.optimum$param), y = global.optimum$value)
      colnames(point.data) = c(par.name, "y")
      pl = pl + geom_point(data = point.data, colour = "tomato")
    }
  }
  if (!is.na(main)) {
    pl = pl + ggtitle(main)
  }
  pl = pl + xlab(par.name)
  return(pl)
}

autoplot2DNumeric = function(x, show.optimum, main, render.contours, render.levels, use.facets, ...) {
  if (!render.levels & !render.contours) {
    stopf("At learst render.contours or render.levels needs to be TRUE. Otherwise we have no data to plot.")
  }

  # extract data
  par.set = getParamSet(x)
  par.names = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)

  # get bounds
  lower = getBounds(getLower(par.set), default = -10L)
  upper = getBounds(getUpper(par.set), default = 10L)

  # build up data frame
  #For example double_sum with x_i in [-65.5, 65.5] takes about 20 minutes to produce the plot
  sequence.x1 = seq(lower[1], upper[1], length.out = 150)
  sequence.x2 = seq(lower[2], upper[2], length.out = 150)
  sequences = list(sequence.x1, sequence.x2)
  data = generateDataframeForGGPlot(x, sequences, par.set)

  # nice color palette for render.levels
  # see http://learnr.wordpress.com/2009/07/20/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-6/
  brewer.div = colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")

  # plot
  pl = ggplot(data = data, mapping = aes_string(x = par.names[1], y = par.names[2]))
  if (render.levels) {
    pl = pl + geom_tile(aes_string(fill = "y"))
    pl = pl + scale_fill_gradientn(colours = brewer.div(200))
    pl = pl + theme(legend.position = "top")
  }
  if (render.contours) {
    pl = pl + stat_contour(aes_string(z = "y", fill = NULL), colour = "gray", alpha = 0.8)
  }

  # show global optimum points
  if (show.optimum && hasGlobalOptimum(x)) {
    df.opt = getGlobalOptimum(x)$param
    df.colnames = colnames(df.opt)
    pl = pl + geom_point(data = df.opt, mapping = aes_string(x = df.colnames[1], y = df.colnames[2]), colour = "tomato")
  }

  # prettify
  pl = pl + xlab(expression(x[1])) + ylab(expression(x[2]))

  if (!is.na(main)) {
    pl = pl + ggtitle(main)
  }
  # pl = pl + scale_x_continuous(expand = c(0,0))
  # pl = pl + scale_y_continuous(expand = c(0,0))

  return(pl)
}
