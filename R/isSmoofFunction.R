#' Checks whether the given object is a \code{smoof_function}.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#' @export
isSmoofFunction = function(object) {
  inherits(object, what = "smoof_function")
}
