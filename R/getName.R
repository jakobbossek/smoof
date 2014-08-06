#' Return name of the function.
#'
#' @template arg_otf_function
#' @return [\code{character(1)}]
#' @export
getName = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "name"))
}