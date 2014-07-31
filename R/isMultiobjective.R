#' Checks whether the given function is multi-objective.
#'
#' @param fn [\code{otf_function}]\cr
#'   Target function.
#' @return [\code{logical(1)}] \code{TRUE} if function is multi-objective.
#' @export
isMultiobjective = function(fn) {
	UseMethod("isMultiobjective")
}

#' @export
isMultiobjective.otf_function = function(fn) {
	return(attr(fn, "n.objectives") >= 2L)
}

#' Checks whether the given function is single-objective.
#'
#' @param fn [\code{otf_function}]\cr
#'   Target function.
#' @return [\code{logical(1)}] \code{TRUE} if function is single-objective.
#' @export
isSingleobjective = function(fn) {
	UseMethod("isSingleobjective")
}

#' @export
isSingleobjective.otf_function = function(fn) {
	return(attr(fn, "n.objectives") == 1L)
}