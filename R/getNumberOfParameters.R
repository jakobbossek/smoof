#' Determine the number of parameters.
#'
#' @template arg_smoof_function
#' @return [\code{integer(1)}]
#' @export
getNumberOfParameters = function(fn) {
	assertClass(fn, "smoof_function")
	return(sum(getParamLengths(getParamSet(fn))))
}