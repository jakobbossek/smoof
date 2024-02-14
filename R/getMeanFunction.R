#' Returns the true mean function in the noisy case.
#'
#' @template arg_smoof_function
#' @return [\code{function}]
#'  True mean function in the noisy case.
#' @export
getMeanFunction = function(fn) {
  UseMethod("getMeanFunction")
}

#' @export
getMeanFunction.smoof_function = function(fn) {
  return(attr(fn, "fn.mean"))
}

#' @export
getMeanFunction.smoof_wrapped_function = function(fn) {
  return(getMeanFunction(getWrappedFunction(fn)))
}
