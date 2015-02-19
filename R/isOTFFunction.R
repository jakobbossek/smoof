#' Checks whether the given object is a smoof_function.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#' @export
issmoofFunction = function(object) {
	inherits(object, what = "smoof_function")
}