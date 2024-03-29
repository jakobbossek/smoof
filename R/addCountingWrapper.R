#' @title
#' Return a function which counts its function evaluations.
#'
#' @description
#' This is a counting wrapper for a \code{smoof_function}, i.e., the returned function
#' first checks whether the given argument is a vector or matrix, saves the
#' number of function evaluations of the wrapped function to compute the function
#' values and finally passes down the argument to the wrapped \code{smoof_function}.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function which should be wrapped.
#' @return [\code{smoof_counting_function}]
#'   The function that includes the counting feature.
#' @examples
#' fn = makeBBOBFunction(dimensions = 2L, fid = 1L, iid = 1L)
#' fn = addCountingWrapper(fn)
#'
#' # We get a value of 0 since the function has not been called yet
#' print(getNumberOfEvaluations(fn))
#'
#' # Now call the function 10 times consecutively
#' for (i in seq(10L)) {
#'   fn(runif(2))
#' }
#' print(getNumberOfEvaluations(fn))
#'
#' # Here we pass a (2x5) matrix to the function with each column representing
#' # one input vector
#' x = matrix(runif(10), ncol = 5L)
#' fn(x)
#' print(getNumberOfEvaluations(fn))
#' @seealso \code{\link{getNumberOfEvaluations}}, \code{\link{resetEvaluationCounter}}
#' @export
addCountingWrapper = function(fn) {
  if (!checkmate::testClass(fn, "smoof_function") && !checkmate::testClass(fn, "smoof_wrapped_function")) {
    BBmisc::stopf("The passed function needs to be a (wrapped) smoof function.")
  }
  force(fn)
  n.evals = 0L
  wrapped.fn = function(x, ...) {
    if (is.matrix(x)) {
      n.evals <<- n.evals + ncol(x)
    } else {
      n.evals <<- n.evals + 1L
    }
    fn(x, ...)
  }
  class(wrapped.fn) = c("smoof_counting_function", "smoof_wrapped_function", "smoof_function", "function")
  return(wrapped.fn)
}
