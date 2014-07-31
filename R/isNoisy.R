#' Checks whether the given function is noisy.
#' 
#' @param fn [\code{otf_function}]\cr
#'   Target function.
#' @return [\code{logical(1)}]
#' @export
isNoisy = function(fn) {
	return(attr(fn, "noisy"))
}