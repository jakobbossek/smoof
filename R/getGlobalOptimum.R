#' Returns the global optimum and its value.
#'
#' @template arg_smoof_function
#' @return [\code{list}] List containing the following entries:
#' \itemize{
#'   \item{param [\code{list}]}{Named list of parameter value(s).}
#'   \item{value [\code{numeric(1)}]}{Optimal value.}
#' }
#' @note Keep in mind, that this method makes sense only for single-objective target function.
#' @export
getGlobalOptimum = function(fn) {
	assertClass(fn, c("smoof_function", "smoof_single_objective_function"))
	if (hasGlobalOptimum(fn)) {
		return(list(
			param = attr(fn, "global.opt.params"),
			value = attr(fn, "global.opt.value")
		))
	}
	NULL
}

#' Checks whether global optimum is known.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
hasGlobalOptimum = function(fn) {
	assertClass(fn, "smoof_function")
	return(!is.null(attr(fn, "global.opt.params")))
}