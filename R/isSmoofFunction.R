#' Checks whether the given object is a \code{smoof_function}.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#' @export
isSmoofFunction = function(object) {
  return(inherits(object, c("smoof_function", "smoof_wrapped_function")))
}
