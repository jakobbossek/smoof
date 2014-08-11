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
		class = c("function", "otf_function", "otf_single_objective_function")
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
	par.set = getParamSet(x)
	n.params = getNumberOfParameters(x)
	if (!isNumeric(par.set, include.int = FALSE) | n.params > 1L) {
		stopf("Currently only 1D numeric functions can be plotted.")
	}
	if (isMultiobjective(x)) {
		stopf("Plotting of multiobjective functions not possible.")
	}

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