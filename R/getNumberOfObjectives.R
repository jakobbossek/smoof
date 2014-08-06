#' Determine the number of objectives.
#'
#' @template arg_otf_function
#' @return [\code{integer(1)}]
#' @export
getNumberOfObjectives = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "n.objectives"))
}