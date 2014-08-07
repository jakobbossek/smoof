#' Checks whether the objective function has constraints.
#'
#' @template arg_otf_function
#' @return [\code{logical(1)}]
#' @export
hasConstraints = function(fn) {
	assertClass(fn, "otf_function")
	(hasBoxConstraints(fn) || hasOtherConstraints(fn))
}

hasBoxConstraints = function(fn) {
	hasFiniteBoxConstraints(getParamSet(fn))
}

hasOtherConstraints = function(fn) {
	!is.null(attr(fn, "constraint.fn"))
}