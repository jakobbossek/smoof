#' Return a function which counts how often it has been evaluated.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function.
#' @return [\code{smoof_function}]
#' @export
addCountingWrapper = function(fn) {
  assertClass(fn, "smoof_function")
  force(fn)
  n.evals = 0L
  fn2 = function(x, ...) {
    if (is.matrix(x)) {
      n.evals <<- n.evals + nrow(x)
    } else {
      n.evals <<- n.evals + 1L
    }
    fn(x, ...)
  }
  attributes(fn2) = attributes(fn)
  class(fn2) = c(class(fn), "smoof_counting_function")
  attributes(fn) = NULL
  return(fn2)
}

#' Return number of function evaluations performed by the given function.
#'
#' @param fn [\code{smoof_counting_function}]\cr
#'   Wrapped \code{smoof_function}.
#' @return [\code{integer(1)}]
#' @export
getNumberOfEvaluations = function(fn) {
  assertClass(fn, c("smoof_function", "smoof_counting_function"))
  return(environment(fn)$n.evals)
}

#' Reset evaluation counter.
#'
#' @param fn [\code{smoof_counting_function}]\cr
#'   Wrapped \code{smoof_function}.
#' @export
resetEvaluationCounter = function(fn) {
  assertClass(fn, c("smoof_function", "smoof_counting_function"))
  environment(fn)$n.evals = 0L
}
