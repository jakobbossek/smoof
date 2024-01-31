#' @title
#' GOMOP function generator.
#'
#' @description
#' Construct a multi-objective function by putting together multiple single-objective
#' smoof functions.
#'
#' @details
#' The decision space of the resulting function is restricted
#' to \eqn{[0,1]^d}. Each parameter \eqn{x} is stretched for each objective function.
#' I.e., if \eqn{f_1, \ldots, f_n} are the single objective smoof functions with
#' box constraints \eqn{[l_i, u_i], i = 1, \ldots, n}, then
#' \deqn{
#'   f(x) = \left(f_1(l_1 + x * (u_1 - l_1)), \ldots, f_1(l_1 + x * (u_1 - l_1))\right)
#' }
#' for \eqn{x \in [0,1]^d} where the additions and multiplication are performed
#' component-wise.
#'
#' @template arg_dimensions
#' @param funs [\code{list}]\cr
#'   List of single-objective smoof functions.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the GOMOP function as a \code{smoof_multi_objective_function} object.
#' @export
makeGOMOPFunction = function(dimensions = 2L, funs = list()) {
  dimensions = checkmate::asCount(dimensions)
  checkmate::assertList(funs, types = "smoof_single_objective_function", min.len = 2L, any.missing = FALSE, all.missing = FALSE)

  n.objectives = length(funs)
  funs.dimensions = sapply(funs, getNumberOfParameters)

  # check compatibility
  fail = which(funs.dimensions != dimensions)
  if (length(fail) > 0L) {
    BBmisc::stopf("GOMOP: All passed single-objective functions need to have dimension %i, but %i functions (%s) do not.",
      dimensions, length(fail), BBmisc::collapse(fail, sep = ", "))
  }

  fn = function(x) {
    # GOMOP function need input argument in [0, 1]^dimension
    checkNumericInput(x, dimensions, lower = 0, upper = 1)
    res = sapply(funs, function(fun) {
      lower = getLowerBoxConstraints(fun)
      upper = getUpperBoxConstraints(fun)
      xx = lower + x * (upper - lower)
      return(fun(xx))
    })
    return(res)
  }

  makeMultiObjectiveFunction(
    name = sprintf("GOMOP function (%s)", BBmisc::collapse(sapply(funs, getName), sep = ", ")),
    id = BBmisc::collapse(sapply(funs, getID), sep = "_"),
    description = "GOMOP function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
      ),
    n.objectives = n.objectives#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeGOMOPFunction) = c("function", "smoof_generator")
attr(makeGOMOPFunction, "name") = c("GOMOP")
attr(makeGOMOPFunction, "type") = c("multi-objective")
attr(makeGOMOPFunction, "tags") = c("multi-objective")
