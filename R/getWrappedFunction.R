#' Get the wrapped function. Useful to get the smoof function, if it is wrapped
#' with e.g. a counting wrapper.
#'
#' @param fn [\code{smoof_wrapped_function}]\cr
#'   Wrapping function.
#' @return [\code{function}]
#' @export
getWrappedFunction = function(fn) {
  assertClass(fn, "smoof_wrapped_function")
  return(environment(fn)$fn)
}
