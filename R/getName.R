#' Return name of the function.
#'
#' @template arg_smoof_function
#' @return [\code{character(1)}]
#' @export
getName = function(fn) {
	assertClass(fn, "smoof_function")
	return(attr(fn, "name"))
}
