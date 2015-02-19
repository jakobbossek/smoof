#' Internal generator for function of smoof type.
#'
#' @param name [\code{character(1)}]\cr
#'   Function name.
#' @param fn [\code{function}]\cr
#'   Target function.
#' @param has.simple.signature [\code{logical(1)}]\cr
#'   Set this to \code{TRUE} if the target function expects a vector as input and \code{FALSE}
#'   if it expects a named list of values. The latter is needed if the function depends on mixed
#'   parameters. Default is \code{TRUE}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set describing different ascpects of the target function parameters, i. e.,
#'   names, lower and/or upper bounds, types and so on. See \code{\link[ParamHelpers]{makeParamSet}}
#'   for further information.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives of the multi-objective function.
#' @param noisy [\code{logical(1)}]\cr
#'   Is the function noisy? Defaults to \code{FALSE}.
#' @param constraint.fn [\code{function | NULL}]\cr
#'   Function which returns a logical vector indicating which indicates whether certain conditions
#'   are met or not. Default is \code{NULL}, which means, that there are no constraints (beside possible)
#'   box constraints defined via the \code{par.set} argument.
#' @return [\code{function}] Target function with additional stuff attached as attributes.
makeObjectiveFunction = function(
	name,
	fn,
	has.simple.signature = TRUE,
	par.set,
	n.objectives,
	noisy = FALSE,
	constraint.fn = NULL) {

	# sanity checks
	assertCharacter(name, len = 1L, any.missing = FALSE)
	assertFunction(fn)
	assertFlag(has.simple.signature, na.ok = FALSE)

	if (has.simple.signature) {
		fn = makeInternalObjectiveFunction(fn)
	}

	assertClass(par.set, "ParamSet")
	assertInt(n.objectives, na.ok = FALSE, lower = 0L)
	assertFlag(noisy, na.ok = FALSE)
	if (!is.null(constraint.fn)) {
		assertFunction(constraint.fn)
	}

	structure(
		fn,
		name = name,
		par.set = par.set,
		noisy = noisy,
		constraint.fn = constraint.fn,
		n.objectives = n.objectives,
		class = c("smoof_function", "function")
	)
}