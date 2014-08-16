# Makes a function which expects a list out of a function which
# expects a vector.
#
# @param fn [\code{function}]\cr
#   R Function or \code{otf_function} which expects a vector of parameters.
# @return [\code{function}]
makeInternalObjectiveFunction = function(fn) {
	force(fn)
	function(x, ...) {
		fn(unlist(x), ...)
	}
}