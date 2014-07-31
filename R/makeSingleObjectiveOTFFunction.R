#' Generator for function of otf type.
#'
#' @param name [\code{character(1)}]\cr
#'   Function name.
#' @param fn [\code{function}]\cr
#'   Target function.
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
makeSingleObjectiveOTFFunction = function(
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
		#FIXME: we have to check for names and types here as well!
		#FIXME: furthermore check if params are within the bounds.
		assertList(global.opt.params)
	}
	if (!is.null(global.opt.value)) {
		#FIXME: later enable discrete functions and stuff like that too.
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
	if (isNoisy(x)) {
		stopf("Currently plotting of 'noisy' functions is not supported.")
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
	param.id = getParamIds(par.set)
	#FIXME: make the stepsize customizable?
	x.grid = seq(lower, upper, by = 0.02)
	data = data.frame(x = x.grid, y = x(x.grid))
	pl = ggplot(data = data, mapping = aes_string(x = "x", y = "y"))
	pl = pl + geom_line()
	pl = pl + ggtitle(paste("Function:", getName(x)))
	pl = pl + xlab(param.id)
	return(pl)
}

# @param n.objectives [\code{integer(1)}]\cr
#   Number of objectives of the target function. Default is single-objective, i. e., \code{n-objectives}
#   equals \code{1}.