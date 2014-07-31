#' Determine the number of parameters.
#'
#' @param fn [\code{otf_function}]\cr
#'   Objective function.
#' @return [\code{integer(1)}]
#' @export
getNumberOfParameters = function(fn) {
	assertClass(fn, "otf_function")
	return(sum(getParamLengths(getParamSet(fn))))
}