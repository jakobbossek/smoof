#' Normalize the function search space by minimal and maximal function value.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Function to normalize.
#' @param min.val [\code{numeric(1)}]\cr
#'   Minimal value.
#' @param max.val [\code{numeric(1)}]\cr
#'   Maximal value.
# @export
normalize = function(fn, min.val, max.val) {
  if (isWrappedSmoofFunction(fn)) {
    stopf("Only unwrapped smoof functions can be normalized.")
  }
  # move attributes
  attrs = attributes(fn)
  attributes(fn) = NULL
  fun2 = function(x, ...) {
    (fn(x, ...) - min.val) / (max.val - min.val)
  }
  attributes(fun2) = attrs
  fun2 = setAttribute(fun2, "global.opt.value", (attrs$global.opt.value - min.val) / (max.val - min.val))
  return(fun2)
}
