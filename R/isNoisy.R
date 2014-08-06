#' Checks whether the given function is noisy.
#' 
#' @template arg_otf_function
#' @return [\code{logical(1)}]
#' @export
isNoisy = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "noisy"))
}