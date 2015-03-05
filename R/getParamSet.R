#' Get parameter set.
#'
#' @template arg_smoof_function
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(fn) {
	assertClass(fn, "smoof_function")
	return(attr(fn, "par.set"))
}
