#' @title
#' Helper function to create a numeric multi-objective optimization test function.
#'
#' @description
#' This is a simplifying wrapper around \code{\link{makeMultiObjectiveFunction}}.
#' It can be used if the function to generate is purely numeric to save some lines
#' of code.
#'
#' @inheritParams makeMultiObjectiveFunction
#' @template arg_par.len
#' @template arg_par.id
#' @template arg_par.lower
#' @template arg_par.upper
#' @return [\code{smoof_multi_objective_function}]
#'  Returns a numeric multi-objective optimization test function created using the provided parameters.
#' 
#' @examples
#' # first we generate the 10d sphere function the long way
#' fn = makeMultiObjectiveFunction(
#'   name = "Testfun",
#'   fn = function(x) c(sum(x^2), exp(sum(x^2))),
#'   par.set = makeNumericParamSet(
#'     len = 10L, id = "a",
#'     lower = rep(-1.5, 10L), upper = rep(1.5, 10L)
#'   ),
#'   n.objectives = 2L
#' )
#'
#' # ... and now the short way
#' fn = mnof(
#'  name = "Testfun",
#'  fn = function(x) c(sum(x^2), exp(sum(x^2))),
#'  par.len = 10L, par.id = "a", par.lower = -1.5, par.upper = 1.5,
#'  n.objectives = 2L
#' )
#' @export
mnof = function(name = NULL,
  id = NULL,
  par.len = NULL,
  par.id = "x",
  par.lower = NULL,
  par.upper = NULL,
  n.objectives,
  description = NULL,
  fn,
  vectorized = FALSE,
  noisy = FALSE,
  fn.mean = NULL,
  minimize = rep(TRUE, n.objectives),
  constraint.fn = NULL,
  ref.point = NULL
  ) {

  checkmate::assertString(par.id, null.ok = TRUE)
  par.len = checkmate::asCount(par.len)
  n.objectives = checkmate::asCount(n.objectives)

  # furhter checks are performed by ParamHelpers
  if (is.null(par.lower))
    par.lower = -Inf
  if (is.null(par.upper))
    par.upper = Inf
  checkmate::assertNumeric(par.lower, min.len = 1L)
  checkmate::assertNumeric(par.upper, min.len = 1L)

  makeMultiObjectiveFunction(
    name = name,
    id = id,
    n.objectives = n.objectives,
    description = description,
    fn = fn,
    has.simple.signature = TRUE, # numeric funs always have a simple signature
    par.set = ParamHelpers::makeNumericParamSet(
      len = par.len,
      id = par.id,
      lower = par.lower,
      upper = par.upper,
      vector = TRUE
    ),
    vectorized = vectorized,
    noisy = noisy,
    fn.mean = fn.mean,
    minimize = minimize,
    constraint.fn = constraint.fn,
    ref.point = ref.point
  )
}
