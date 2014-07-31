#' Checks whether the given function is multiobjective.
#'
#' @param fn [\code{otf_function}]\cr
#'   Target function.
#' @return [\code{logical(1)}] \code{TRUE} if function is multiobjective.
#' @export
isMultiobjective = function(fn) {
	UseMethod("isMultiobjective")
}

#' @export
isMultiobjective.otf_function = function(fn) {
	return(attr(fn, "n.objectives") >= 2L)
}