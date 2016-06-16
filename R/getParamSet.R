#' @title Get parameter set.
#'
#' @description
#' Each smoof function contains a parameter set of type \code{\link[ParamHelpers]{ParamSet}}
#' assigned to it, which describes types and bounds of the function parameters.
#' This function returns the parameter set.
#'
#' @template arg_smoof_function
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @examples
#' fn = makeSphereFunction(3L)
#' ps = getParamSet(fn)
#' print(ps)
#' @name getParamSet
#' @rdname getParamSet
NULL

#' @export
getParamSet.smoof_function = function(x) {
  return(attr(x, "par.set"))
}

#' @export
getParamSet.smoof_wrapped_function = function(x) {
  return(getParamSet(getWrappedFunction(x)))
}
