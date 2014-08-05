# Checks if all values of a given list is within bounds.
#
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param x [\code{list}]\cr
#   List of values in the corresponding order.
# @return [\code{logical(1)}]
inBounds = function(par.set, x) {
	assertClass(par.set, "ParamSet")
	assertClass(x, "list")
	if (sum(getParamLengths(par.set)) != length(x)) {
		stopf("Parameter set and values not of the same length.")
	}
	all(unlist(Map(function(par, val) {
		if (isNumeric(par, include.int = TRUE)) {
			return((val >= par$lower) && (val <= par$upper))
		}
		if (isDiscrete(par)) {
			return(as.character(val) %in% unlist(par$values))
		}
	}, par.set$pars, x)))
}