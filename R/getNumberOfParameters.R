#' Determines the number of parameters.
#'
#' @template arg_smoof_function
#' @return [\code{integer(1)}]
#'  The number of parameters.
#' @export
getNumberOfParameters = function(fn) {
  UseMethod("getNumberOfParameters")
}

#' @export
getNumberOfParameters.smoof_function = function(fn) {
  return(sum(ParamHelpers::getParamLengths(getParamSet(fn))))
}

#' @export
getNumberOfParameters.smoof_wrapped_function = function(fn) {
  return(getNumberOfParameters(getWrappedFunction(fn)))
}
