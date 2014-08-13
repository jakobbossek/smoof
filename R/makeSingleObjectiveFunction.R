#' Generator for function of otf type.
#'
#' @param name [\code{character(1)}]\cr
#'   Function name.
#' @template arg_fn
#' @param has.simple.signature [\code{logical(1)}]\cr
#'   Set this to \code{TRUE} if the target function expects a vector as input and \code{FALSE}
#'   if it expects a named list of values. The latter is needed if the function depends on mixed
#'   parameters. Default is \code{TRUE}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set describing different ascpects of the target function parameters, i. e.,
#'   names, lower and/or upper bounds, types and so on. See \code{\link[ParamHelpers]{makeParamSet}}
#'   for further information.
#' @param noisy [\code{logical(1)}]\cr
#'   Is the function noisy? Defaults to \code{FALSE}.
#' @param constraint.fn [\code{function | NULL}]\cr
#'   Function which returns a logical vector indicating which indicates whether certain conditions 
#'   are met or not. Default is \code{NULL}, which means, that there are no constraints (beside possible)
#'   box constraints defined via the \code{par.set} argument.
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

	# sanity checks
	assertCharacter(name, len = 1L, any.missing = FALSE)
	assertFunction(fn)
	assertFlag(has.simple.signature, na.ok = FALSE)

	# Makes a function which expects a list out of a function which
	# expects a vector.
	makeInternalObjectiveFunction = function(fn) {
		force(fn)
		function(x, ...) {
			fn(unlist(x), ...)
		}
	}

	if (has.simple.signature) {
		fn = makeInternalObjectiveFunction(fn)
	}

	assertClass(par.set, "ParamSet")
	assertFlag(noisy, na.ok = FALSE)
	if (!is.null(constraint.fn)) {
		assertFunction(constraint.fn)
	}
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
		global.opt.value = fn(global.opt.params)
		assertNumber(global.opt.value, na.ok = FALSE, finite = TRUE)
	}

	structure(
		fn, 
		name = name,
		par.set = par.set,
		noisy = noisy,
		constraint.fn = constraint.fn,
		n.objectives = 1L,
		global.opt.params = global.opt.params,
		global.opt.value = global.opt.value,
		class = c("otf_function", "otf_single_objective_function", "function")
	)
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
			stop("2D numeric plot not finished yet.")
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
getBound = function(bound, default) {
	if (is.infinite(bound))
		return(default)
	return(bound)
}

autoplot1DNumeric = function(x, ...) {
	# extract data
	par.set = getParamSet(x)
	par.name = getParamIds(par.set)

	# get lower and upper bounds
	lower = getBound(bound = getLower(par.set), default = -10L)
	upper = getBound(bound = getUpper(par.set), default = 10L)

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
	pl = pl + ggtitle(paste("Function:", getName(x)))
	pl = pl + xlab(par.name)
	return(pl)
}

autoplot2DNumeric = function(x, ...) {
	stopf("Not implemented yet.")
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
	lower = getBound(bound = getLower(par.set), default = -10L)
	upper = getBound(bound = getUpper(par.set), default = 10L)

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
	pl = pl + ggtitle(paste("Function:", getName(x)))
	pl = pl + theme(legend.position = "top")

	return(pl)
}