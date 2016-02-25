# Get data.frame of optima.
#
# Returns data frame of global / local optima for ggplot.
# @param fn [\code{smoof_function}]\cr
#   Smoof function.
# @return [\code{data.frame}]
getOptimaDf = function(fn) {
  df = data.frame()
  # local optima first since the global opt is a local opt as well and
  # would thus not be visible in the plot
  if (hasLocalOptimum(fn)) {
    loc.opt = getLocalOptimum(fn)
    loc.opt = cbind(loc.opt$params, data.frame(y = loc.opt$values, optima = rep("local", length(loc.opt$values))))
    df = rbind(df, loc.opt)
  }
  if (hasGlobalOptimum(fn)) {
    glob.opt = getGlobalOptimum(fn)
    glob.opt = cbind(glob.opt$param, data.frame(y = rep(glob.opt$value, nrow(glob.opt$param)), optima = rep("global", nrow(glob.opt$param))))
    df = rbind(df, glob.opt)
  }
  if (nrow(df) == 0) {
    return(NULL)
  }
  df$optima = as.factor(df$optima)
  return(df)
}


# Utility function.
#
# Generates 'gg-plotable' data.frame.
# @param fn [\code{smoof_function}]\cr
#   Target function.
# @param sequences [\code{list}]\cr
#   List of sequences. One sequence for each parameter.
#   Unified with expand.grid.
# @param par.set [\code{ParamSet}]\cr
#   Parameter set.
# @return [\code{data.frame}]
generateDataframeForGGPlot = function(fn, sequences, par.set) {
  data = do.call(expand.grid, sequences)
  colnames(data) = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  data.as.list = dfRowsToList(par.set = par.set, df = data)
  data[["y"]] = sapply(data.as.list, function(data.row) {
    if (violatesConstraints(fn, unlist(data.row))) {
      return(NA)
    }
    return(fn(data.row))
  })
  return(data)
}

# Generate data frame for ggplot2.
#
# Gets a function and returns a data.frame evaluated on a large grid
# of data points in order to visualize functions with 2d numeric and
# up to 4d functions with >= 1 numeric and >=2 discrete parameters.
#
# @param fun [\code{smoof_function}]\cr
#   Smoof function.
# @param length.out [\code{integer(1)}]\cr
#   Desired length of sequences for numeric parameters.
# @return [\code{data.frame}]
generateDataframeForGGPlot2 = function(fun, length.out = 50L) {
  # extract a bunch of parameter information
  par.set = getParamSet(fun)
  par.types = getParamTypes(par.set)
  pars = par.set$pars
  n.pars = length(pars)

  # build data.frame
  # This is kind of tedious since we want support for a large variety of parameters
  # combinations, e.g., {numeric, discrete}, {numeric, discrete, logical},
  # {discretevector, numericvector}, ...
  # Note: values is a list of lists
  values = lapply(1:length(pars), function(i) {
    the.par = pars[[i]]
    par.name = names(pars)[i]
    par.type = par.types[i]
    values = NULL
    if (par.type == "numeric") {
      values = seq(the.par$lower, the.par$upper, length.out = length.out)
    } else if (par.type == "numericvector") {
      values = lapply(1:the.par$len, function(i) {
        seq(the.par$lower[i], the.par$upper[i], length.out = length.out)
      })
    } else if (par.type == "discrete") {
      values = unlist(the.par$values, use.names = FALSE)
    } else if (par.type == "discretevector") {
      values = lapply(1:the.par$len, function(i) {
        unlist(the.par$values, use.names = FALSE)
      })
    } else if (par.type == "logical") {
      values = as.character(unlist(the.par$values, use.names = FALSE))
    } else if (par.type == "logicalvector") {
      values = lapply(1:the.par$len, function(i) {
        as.character(unlist(the.par$values, use.names = FALSE))
      })
    }
    return(values)
  })

  # convert a list of lists of objects into a list of objects, e.g.,
  # list(a, b, list(c, d), e) |-> list(a, b, c, d, e)
  flatten = function(x) {
    if (is.list(x)) {
      Reduce(c, lapply(x, flatten))
    } else {
      list(x)
    }
  }

  # list of lists -> list of vectors
  values = flatten(values)

  # finally build the grid
  grid = do.call(expand.grid, values)
  colnames(grid) = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)

  # now compute the function values and append
  #FIXME: check if one of the parameters is named "y"

  grid2 = dfRowsToList(par.set = par.set, df = grid, enforce.col.types = TRUE)
  grid[, "y"] = sapply(grid2, fun)

  return(grid)
}

# Utility function.
#
# Get actual bound if finite or default value for plotting.
# @param bound [\code{numeric(1)}]\cr
#   Numeric bound.
# @param default [\code{numeric(1)}]\cr
#   Default value. Used if bound is infinite.
# @return [\code{numeric(1)}]
getBounds = function(bound, default) {
  if (any(is.infinite(bound)))
    return(rep(default, length(bound)))
  return(bound)
}

# Check if plotting is possible.
#
# @param x [\code{smoof_function}]\cr
#  Smoof function.
# @return Nothing
checkPlotFunParams = function(x) {
  n.params = getNumberOfParameters(x)
  par.set = getParamSet(x)

  # if (n.params > 2L) {
  #   stopf("Only function with up to 2 parameters can be plotted, but your function has %i", n.params)
  # }

  if (isMultiobjective(x)) {
    stopf("Plotting of multiobjective functions not possible.")
  }
}

# Map number of params to the corresponding plot function.
#
# @param x [\code{smoof_function}]\cr
#   Smoof Function.
# @param mapping [\code{list}]\cr
#   Mapping from string to function.
# @return [\code{function}]
getInternalPlotFunction = function(x, mapping) {
  n.params = getNumberOfParameters(x)
  par.set = getParamSet(x)

  if (isNumeric(par.set, include.int = FALSE)) {
    if (n.params == 1L) {
      return(mapping[["1Dnumeric"]])
    } else {
      return(mapping[["2Dnumeric"]])
    }
  } else if (hasDiscrete(par.set) & hasNumeric(par.set, include.int = FALSE)) {
    return(mapping[["2DMixed"]])
  }
  stopf("This type of function cannot be plotted.")
}
