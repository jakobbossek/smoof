#' Get parameter set.
#'
#' @param fn [\code{otf_function}]\cr
#'   Objective function.
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "par.set"))
}