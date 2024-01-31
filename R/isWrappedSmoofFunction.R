#' Checks whether the function is of type \code{smoof_wrapped_function}.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#'  \code{TRUE} if the object is a SMOOF wrapped function, \code{FALSE} otherwise.
#' @export
isWrappedSmoofFunction = function(object) {
  inherits(object, what = "smoof_wrapped_function")
}
