#' Determine the number of parameters.
#'
#' @param fn [\code{otf_function}]\cr
#'   Objective function.
#' @return [\code{integer(1)}]
#' @export
getNumberOfParameters = function(fn) {
	return(sum(getParamLengths(attr(fn, "par.set"))))
}