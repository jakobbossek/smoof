#' Checks whether the given object is a otf_function.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#' @export
isOTFFunction = function(object) {
	inherits(object, what = "otf_function")
}