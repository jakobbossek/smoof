#' Determine the number of objectives.
#'
#' @template arg_smoof_function
#' @return [\code{integer(1)}]
#' @export
getNumberOfObjectives = function(fn) {
	assertClass(fn, "smoof_function")
	return(attr(fn, "n.objectives"))
}