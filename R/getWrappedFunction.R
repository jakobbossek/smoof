#' Extract wrapped function.
#'
#' The \pkg{smoof} package offers means to let a function log its evaluations or
#' even store to points and function values it has been evaluated on. This is done
#' by wrapping the function with other functions. This helper function extract
#' the wrapped function.
#'
#' @param fn [\code{smoof_wrapped_function}]\cr
#'   Wrapping function.
#' @return [\code{function}]
#' @seealso \code{\link{addCountingWrapper}}, \code{\link{addLoggingWrapper}}
#' @export
getWrappedFunction = function(fn) {
  assertClass(fn, "smoof_wrapped_function")
  return(environment(fn)$fn)
}
