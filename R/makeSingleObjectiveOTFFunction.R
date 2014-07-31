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
	structure(
		fn, 
		name = name,
		par.set = par.set,
		noisy = noisy,
		global.opt.params = global.opt.params,
		global.opt.value = global.opt.value,
		class = c("otf_function", "otf_single_objective_function"))
}

# @param n.objectives [\code{integer(1)}]\cr
#   Number of objectives of the target function. Default is single-objective, i. e., \code{n-objectives}
#   equals \code{1}.