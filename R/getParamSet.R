#' Get parameter set.
#'
#' @template arg_smoof_function
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(fn) {
  UseMethod("getParamSet")
}

#' @export
getParamSet.smoof_function = function(fn) {
  return(attr(fn, "par.set"))
}

#' @export
getParamSet.smoof_wrapped_function = function(fn) {
  return(getParamSet(getWrappedFunction(fn)))
}
