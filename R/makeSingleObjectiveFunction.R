#' Generator for function of otf type.
#'
#' @param name [\code{character(1)}]\cr
#'   Function name.
#' @template arg_fn
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set describing different ascpects of the target function parameters, i. e.,
#'   names, lower and/or upper bounds, types and so on. See \code{\link[ParamHelpers]{makeParamSet}}
#'   for further information.
#' @param noisy [\code{logical(1)}]\cr
#'   Is the function noisy? Defaults to \code{FALSE}.
#' @param global.opt.params [\code{list}]\cr
#'   List of named parameter values of the global optimum. Default is \code{NULL} which means unknown.
#' @param global.opt.value [\code{numeric(1)}]\cr
#'   Global optimum value if known. Default is \code{NULL}.
#' @return [\code{function}] Target function with additional stuff attached as attributes.
#' @export
#FIXME: we should force the user to provide a (named) list of parameter values to the target function
#       In simple cases (all parameters numeric) the constructor should have the option params.as.vector
#       with default FALSE.
makeSingleObjectiveFunction = function(
	name,
	fn,
	par.set,
	noisy = FALSE,
	global.opt.params = NULL,
	global.opt.value = NULL) {

	# sanity checks
	assertCharacter(name, len = 1L, any.missing = FALSE)
	assertFunction(fn)
	assertClass(par.set, "ParamSet")
	assertFlag(noisy, na.ok = FALSE)
	if (!is.null(global.opt.params)) {
		assertList(global.opt.params)
		if (!inBounds(par.set, global.opt.params)) {
			stopf("Global optimum out of bounds.")
		}
		if (!setequal(getParamIds(par.set), names(global.opt.params))) {
			stopf("Names of values and parameter names do not match.")
		}
	}
	if (is.null(global.opt.value) && !is.null(global.opt.params)) {
		messagef("Parameter values for global optimum provided, but not the optimal value. Commputing.")
		#FIXME: later enable discrete functions and stuff like that too.
		#FIXME: the 'unlisting' works only for numeric functions. For mixed
		#       parameter functions we need to to pass a list!
		global.opt.value = fn(unlist(global.opt.params))
		assertNumber(global.opt.value, na.ok = FALSE, finite = TRUE)
	}

	structure(
		fn, 
		name = name,
		par.set = par.set,
		noisy = noisy,
		n.objectives = 1L,
		global.opt.params = global.opt.params,
		global.opt.value = global.opt.value,
		class = c("otf_function", "otf_single_objective_function"))
}

#' @export
print.otf_function = function(x, ...) {
	n.objectives.text = ifelse(isSingleobjective(x), "Single", "Multi")
	catf("%s-objective function.", n.objectives.text)
	if (isMultiobjective(x)) {
		catf("Number of objectives: %i", getNumberOfObjectives(x))
	}
	catf("Noisy: %s", as.character(isNoisy(x)))
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
		data.frame(x = x.grid, y = sapply(x.grid, fn))
	}

	data = getPlotData(x, lower, upper)
	param.id = getParamIds(par.set)
	#FIXME: make the stepsize customizable?
	pl = ggplot(data = data, mapping = aes_string(x = "x", y = "y"))
	if (isNoisy(x)) {
		pl = pl + geom_point()
	} else {		
		pl = pl + geom_line()
		if (!is.null(attr(x, "global.opt.params"))) {
			pl = pl + geom_vline(xintercept = as.numeric(attr(x, "global.opt.params")), linetype = "dashed", colour = "grey")
			point.data = data.frame(x = unlist(attr(x, "global.opt.params")), y = attr(x, "global.opt.value"))
			pl = pl + geom_point(data = point.data, colour = "tomato")
		}
	}
	pl = pl + ggtitle(paste("Function:", getName(x)))
	pl = pl + xlab(param.id)
	return(pl)
}

# @param n.objectives [\code{integer(1)}]\cr
#   Number of objectives of the target function. Default is single-objective, i. e., \code{n-objectives}
#   equals \code{1}.