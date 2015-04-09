#' Return a function which counts how often it has been evaluated.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function.
#' @return [\code{smoof_counting_function}]
#' @export
addCountingWrapper = function(fn) {
  assertClass(fn, "smoof_function")
  force(fn)
  n.evals = 0L
  wrapped.fn = function(x, ...) {
    if (is.matrix(x)) {
      n.evals <<- n.evals + nrow(x)
    } else {
      n.evals <<- n.evals + 1L
    }
    fn(x, ...)
  }
  class(wrapped.fn) = c("smoof_counting_function", "smoof_wrapped_function")
  return(wrapped.fn)
}
