#' @title
#' BK1 function generator
#'
#' @description
#' Generates the BK1 function, a multi-objective optimization test function. The BK1 function
#' is commonly used in benchmarking studies for evaluating the performance of optimization algorithms.
#'
#' @references
#' ...
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the BK1 function as a \code{smoof_multi_objective_function} object.
#' @export
makeBK1Function = function() {

  # C++ implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    return(.Call("mof_bk1", x))
  }

  makeMultiObjectiveFunction(
    name = "BK1 Function",
    id = "bk1",
    description = "BK1 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-5, 2L),
      upper = rep(10, 2L),
      vector = TRUE
      ),
    n.objectives = 2L#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeBK1Function) = c("function", "smoof_generator")
attr(makeBK1Function, "name") = c("BK1")
attr(makeBK1Function, "type") = c("multi-objective")
attr(makeBK1Function, "tags") = c("multi-objective")
