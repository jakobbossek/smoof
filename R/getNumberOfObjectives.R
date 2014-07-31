#' Determine the number of objectives.
#'
#' @param fn [\code{otf_function}]\cr
#'   Objective function.
#' @return [\code{integer(1)}]
#' @export
getNumberOfObjectives = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "n.objectives"))
}