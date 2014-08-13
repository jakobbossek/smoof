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
#'   box constraints.
#' @param global.opt.params [\code{list}]\cr
#'   List of named parameter values of the global optimum. Default is \code{NULL} which means unknown.
#' @param global.opt.value [\code{numeric(1)}]\cr
#'   Global optimum value if known. Default is \code{NULL}.
#' @return [\code{function}] Target function with additional stuff attached as attributes.
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


autoplot1DNumeric = function(x, ...) {
	par.set = getParamSet(x)

	lower = getLower(par.set)
	upper = getUpper(par.set)
	if (is.infinite(lower)) {
		lower = -10L
	}
	if (is.infinite(upper)) {
		upper = 10L
	}

	getPlotData = function(fn, lower, upper, stepsize = ifelse(isNoisy(fn), 0.08, 0.01)) {
		x.grid = seq(lower, upper, by = stepsize)
		data.frame(x = x.grid, y = sapply(x.grid, function(x) {
				fn(namedList(init = x, names = getParamIds(par.set)))
			})
		)
	}

	data = getPlotData(x, lower, upper)
	param.id = getParamIds(par.set)
	#FIXME: make the stepsize customizable?
	pl = ggplot(data = data, mapping = aes_string(x = "x", y = "y"))
	if (isNoisy(x)) {
		pl = pl + geom_point()
	} else {		
		pl = pl + geom_line()
		if (hasGlobalOptimum(x)) {
			global.optimum = getGlobalOptimum(x)
			pl = pl + geom_vline(xintercept = as.numeric(global.optimum$param), linetype = "dashed", colour = "grey")
			point.data = data.frame(x = unlist(global.optimum$param), y = global.optimum$value)
			pl = pl + geom_point(data = point.data, colour = "tomato")
		}
	}
	pl = pl + ggtitle(paste("Function:", getName(x)))
	pl = pl + xlab(param.id)
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

	#FIXME: copy & paste. Maybe offer method getLower(par.set)
	lower = getLower(par.set)
	upper = getUpper(par.set)
	if (is.infinite(lower)) {
		lower = -10L
	}
	if (is.infinite(upper)) {
		upper = 10L
	}

	numeric.seq = seq(lower, upper, by = 0.01)
	#FIXME: 'getValues' for Params?
	factor.seq = unlist(par.set$pars[[idx.factor]]$values)

	# build up data frame
	#FIXME: this is tedious and cumbersome. Export to nice functions which is 
	#       used by all autoplot subroutines
	data = expand.grid(numeric.seq, factor.seq, stringsAsFactors = TRUE)
	names(data) = par.names

	pars.as.list = dfRowsToList(par.set = par.set, df = data)
	data[["y"]] = sapply(pars.as.list, function(par) x(par))

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