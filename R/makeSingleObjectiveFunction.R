#' Generator for single-objective target functions.
#'
#' @template arg_name
#' @template arg_fn
#' @template arg_has_simple_signature
#' @template arg_par_set
#' @template arg_noisy
#' @template arg_constraint_fn
#' @param global.opt.params [\code{list}]\cr
#'   List of named parameter values of the global optimum. Default is \code{NULL} which means unknown.
#' @param global.opt.value [\code{numeric(1)}]\cr
#'   Global optimum value if known. Default is \code{NULL}.
#' @return [\code{function}] Target function with additional stuff attached as attributes.
#' @examples
#'   library(ggplot2)
#'
#'   fn = makeSingleObjectiveFunction(
#'     name = "Sphere Function",
#'     fn = function(x) sum(x^2),
#'     par.set = makeNumericParamSet("x", len = 1L, lower = -5L, upper = 5L),
#'     global.opt.params = list(x = 0)
#'   )
#'   print(fn)
#'   print(autoplot(fn))
#'
#'   fn.num2 = makeSingleObjectiveFunction(
#'   	name = "Numeric 2D",
#'  	fn = function(x) sum(x^2),
#'  	par.set = makeParamSet(
#'  		makeNumericParam("x1", lower = -5, upper = 5),
#'  		makeNumericParam("x2", lower = -5, upper = 5)
#'  	)
#'   )
#'   print(fn)
#'   print(autoplot(fn))
#'  
#'   fn.mixed = makeSingleObjectiveFunction(
#'  	name = "Mixed 2D",
#'  	fn = function(x) x$num1^2 + as.integer(as.character(x$disc1) == "a"),
#'  	has.simple.signature = FALSE,
#'  	par.set = makeParamSet(
#'  		makeNumericParam("num1", lower = -5, upper = 5),
#'  		makeDiscreteParam("disc1", values = c("a", "b"))
#'  	),
#'  	global.opt.params = list(num1 = 0, disc1 = "b")
#'   ) 
#'   print(fn)
#'   print(autoplot(fn))
#' @export
makeSingleObjectiveFunction = function(
	name,
	fn,
	has.simple.signature = TRUE,
	par.set,
	noisy = FALSE,
	constraint.fn = NULL,
	global.opt.params = NULL,
	global.opt.value = NULL) {

	otf.fn = makeObjectiveFunction(name, fn, has.simple.signature, par.set, 1L, noisy, constraint.fn)

	if (!is.null(global.opt.params)) {
		assertList(global.opt.params)
		if (!isFeasible(par.set, global.opt.params)) {
			stopf("Global optimum out of bounds.")
		}

		#FIXME: should we allow unnamed lists?
		if (!setequal(getParamIds(par.set), names(global.opt.params))) {
			stopf("Names of values and parameter names do not match.")
		}
	}
	if (is.null(global.opt.value) && !is.null(global.opt.params)) {
		messagef("Computing optimal value, because just the parameters of the global optimum provided.")
		#FIXME: later enable discrete functions and stuff like that too.
		global.opt.value = otf.fn(global.opt.params)
		assertNumber(global.opt.value, na.ok = FALSE, finite = TRUE)
	}

	otf.fn = setAttribute(otf.fn, "global.opt.params", global.opt.params)
	otf.fn = setAttribute(otf.fn, "global.opt.value", global.opt.value)

	class(otf.fn) = c("otf_single_objective_function", class(otf.fn))

	return(otf.fn)
}

#' @export
print.otf_function = function(x, ...) {
	n.objectives.text = ifelse(isSingleobjective(x), "Single", "Multi")
	catf("%s-objective function.", n.objectives.text)
	if (isMultiobjective(x)) {
		catf("Number of objectives: %i", getNumberOfObjectives(x))
	}
	catf("Noisy: %s", as.character(isNoisy(x)))
	catf("Constraints: %s", as.character(hasConstraints(x)))
	catf("Number of parameters: %i", getNumberOfParameters(x))
	print(getParamSet(x))
}

#' @export
autoplot.otf_function = function(x, ...) {
	n.params = getNumberOfParameters(x)
	par.set = getParamSet(x)

	if (n.params > 2L) {
		stopf("Only function with up to 2 parameters can be plotted, but your function has %i", n.params)
	}

	if (isMultiobjective(x)) {
		stopf("Plotting of multiobjective functions not possible.")
	}

	autoplotFun = NULL
	if (isNumeric(par.set, include.int = FALSE)) {
		if (n.params == 1L) {
			autoplotFun = autoplot1DNumeric
		} else {
			autoplotFun = autoplot2DNumeric
		}
	} else if (hasDiscrete(par.set) & hasNumeric(par.set, include.int = FALSE)) {
		autoplotFun = autoplot2DMixed
	} else {
		stopf("This type of function cannot be plotted.")
	}

	autoplotFun(x, ...)
}

# Utility functions.
#
# Generates 'gg-plotable' data.frame.
# @param fn [\code{otf_function}]\cr
#   Target function.
# @param sequences [\code{list}]\cr
#   List of sequences. One sequence for each parameter.
#   Unified with expand.grid.
# @param par.set [\code{ParamSet}]\cr
#   Parameter set.
# @return [\code{data.frame}]
generateDataframeForGGPlot = function(fn, sequences, par.set) {
	data = do.call(expand.grid, sequences)
	colnames(data) = getParamIds(par.set)
	data.as.list = dfRowsToList(par.set = par.set, df = data)
	data[["y"]] = sapply(data.as.list, function(data.row) fn(data.row))
	return(data)
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

autoplot1DNumeric = function(x, ...) {
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
		if (hasGlobalOptimum(x)) {
			global.optimum = getGlobalOptimum(x)
			pl = pl + geom_vline(xintercept = as.numeric(global.optimum$param), linetype = "dashed", colour = "grey")
			point.data = data.frame(x = unlist(global.optimum$param), y = global.optimum$value)
			colnames(point.data) = c(par.name, "y")
			pl = pl + geom_point(data = point.data, colour = "tomato")
		}
	}
	pl = pl + ggtitle(getName(x))
	pl = pl + xlab(par.name)
	return(pl)
}

autoplot2DNumeric = function(x, render.levels = FALSE, render.contours = TRUE, ...) {
	assertFlag(render.levels, na.ok = FALSE)
	assertFlag(render.contours, na.ok = FALSE)

	if (!render.levels & !render.contours)
		stopf("At learst render.contours or render.levels needs to be TRUE. Otherwise we have no data to plot.")

	# extract data
	par.set = getParamSet(x)
	par.names = getParamIds(par.set)

	# get bounds
	lower = getBounds(getLower(par.set), default = -10L)
	upper = getBounds(getUpper(par.set), default = 10L)

	# build up data frame
	#FIXME: setting the stepsize (for example b = 0.05) is very evil!
	#For example double_sum with x_i in [-65.5, 65.5] takes about 20 minutes to produce the plot
	sequence.x1 = seq(lower[1], upper[1], length.out = 150)
	sequence.x2 = seq(lower[2], upper[2], length.out = 150)
	sequences = list(sequence.x1, sequence.x2)
	data = generateDataframeForGGPlot(x, sequences, par.set)

	# nice color palette for render.levels
	# see http://learnr.wordpress.com/2009/07/20/ggplot2-version-of-figures-in-lattice-multivariate-data-visualization-with-r-part-6/
	brewer.div <- colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")

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
	pl = pl + xlab(expression(x[1])) + ylab(expression(x[2]))
	pl = pl + ggtitle(getName(x))
	# pl = pl + scale_x_continuous(expand = c(0,0))
	# pl = pl + scale_y_continuous(expand = c(0,0))

    return(pl)
}

autoplot2DMixed = function(x, use.facets = FALSE, ...) {
	assertFlag(use.facets)

	# extract data
	par.set = getParamSet(x)
	par.types = getParamTypes(par.set)
	par.names = getParamIds(par.set)

	# which parameter is discrete/logical?
	idx.factor = which(par.types %in% c("discrete", "logical"))
	idx.numeric = setdiff(1:2, idx.factor)

	# get names of factors respectively numeric parameters
	name.factor = par.names[idx.factor]
	name.numeric = par.names[idx.numeric]

	# get bounds
	lower = getBounds(bound = getLower(par.set), default = -10L)
	upper = getBounds(bound = getUpper(par.set), default = 10L)

	numeric.seq = seq(lower, upper, by = 0.01)
	#FIXME: 'getValues' for Params?
	factor.seq = unlist(par.set$pars[[idx.factor]]$values)
	sequences = list(numeric.seq, factor.seq)
	if (idx.factor == 1L) {
		sequences = list(factor.seq, numeric.seq)
	}

	# build up data frame
	data = generateDataframeForGGPlot(fn = x, sequences = sequences, par.set = par.set)

	pl = ggplot(data = data, mapping = aes_string(x = name.numeric, y = "y"))
	if (use.facets) {
		pl = pl + geom_line()
		pl = pl + facet_grid(reformulate(".", name.factor))
	} else {
		pl = pl + geom_line(aes_string(linetype = name.factor))
	}
	pl = pl + ggtitle(getName(x))
	pl = pl + theme(legend.position = "top")

	return(pl)
}