#' Get parameter set.
#'
#' @template arg_otf_function
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(fn) {
	assertClass(fn, "otf_function")
	return(attr(fn, "par.set"))
}