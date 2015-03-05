#' Checks whether the objective function has constraints.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
hasConstraints = function(fn) {
	assertClass(fn, "smoof_function")
	(hasBoxConstraints(fn) || hasOtherConstraints(fn))
}

#' Checks whether the objective function has box constraints.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
hasBoxConstraints = function(fn) {
	hasFiniteBoxConstraints(getParamSet(fn))
}

#' Checks whether the objective function has other constraints.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
hasOtherConstraints = function(fn) {
	!is.null(attr(fn, "constraint.fn"))
}
