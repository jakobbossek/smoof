#' Get otf_function name.
#'
#' @param fn [\code{otf_function}]\cr
#'   Objective function.
#' @return [\code{character(1)}]
#' @export
getName = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "name"))
}