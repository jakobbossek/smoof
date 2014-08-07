#' Checks whether constraints are violated.
#'
#' @template arg_otf_function
#' @param values [\code{numeric}]\cr
#'   List of values.
#' @return [\code{logical(1)}]
#FIXME: untested and thus not exported
#FIXME: if the function expects not only numeric values, we need to pass a _list_ of values
#       and not a (numeric) vector.
violatesConstraints = function(fn, values) {
	assertClass(fn, "otf_function")
	assertNumeric(values, any.missing = FALSE)
	constraint.fn = attr(fn, "constraint.fn")
	!all(constraint.fn(values))
}