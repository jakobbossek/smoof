#' Generator for multi-objective target functions.
#'
#' @template arg_name
#' @template arg_description
#' @template arg_fn
#' @template arg_has_simple_signature
#' @template arg_par_set
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives of the multi-objective function.
#' @template arg_noisy
#' @template arg_constraint_fn
#' @return [\code{function}] Target function with additional stuff attached as attributes.
#' @examples
#' fn = makeMultiObjectiveFunction(
#'   name = "Sphere Function",
#'   fn = function(x) c(sum(x^2), exp(x)),
#'   n.objectives = 2L,
#'   par.set = makeNumericParamSet("x", len = 1L, lower = -5L, upper = 5L)
#' )
#' print(fn)
#' @export
makeMultiObjectiveFunction = function(
	name,
	description = NULL,
	fn,
	has.simple.signature = TRUE,
	par.set,
	n.objectives,
	noisy = FALSE,
	constraint.fn = NULL) {

	smoof.fn = makeObjectiveFunction(name, description, fn, has.simple.signature, par.set, n.objectives, noisy, constraint.fn)

	class(smoof.fn) = c("smoof_multi_objective_function", class(smoof.fn))

	return(smoof.fn)
}
