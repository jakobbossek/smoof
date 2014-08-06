#' Determine the number of parameters.
#'
#' @template arg_otf_function
#' @return [\code{integer(1)}]
#' @export
getNumberOfParameters = function(fn) {
	assertClass(fn, "otf_function")
	return(sum(getParamLengths(getParamSet(fn))))
}