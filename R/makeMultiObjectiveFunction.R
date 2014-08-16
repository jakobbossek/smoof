#' Generator for function of otf type.
#'
#' @param name [\code{character(1)}]\cr
#'   Function name.
#' @template arg_fn
#' @template arg_has_simple_signature
#' @template arg_par_set
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives of the multi-objective function.
#' @template arg_noisy
#' @template arg_constraint_fn
#' @return [\code{function}] Target function with additional stuff attached as attributes.
#' @examples
#'
#'   fn = makeMultiObjectiveFunction(
#'     name = "Sphere Function",
#'     fn = function(x) c(sum(x^2), exp(x)),
#'     par.set = makeNumericParamSet("x", len = 1L, lower = -5L, upper = 5L)
#'   )
#'   print(fn)
#' @export
makeMultiObjectiveFunction = function(
	name,
	fn,
	has.simple.signature = TRUE,
	par.set,
	n.objectives,
	noisy = FALSE,
	constraint.fn = NULL) {

	otf.fn = makeObjectiveFunction(name, fn, has.simple.signature, par.set, n.objectives, noisy, constraint.fn)

	class(otf.fn) = c("otf_single_objective_function", class(otf.fn))

	return(otf.fn)
}
