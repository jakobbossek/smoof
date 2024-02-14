#' @title
#' MOP7 function generator.
#'
#' @description
#' MOP7 function from Van Valedhuizen's test suite.
#'
#' @references
#' R. Viennet, C. Fonteix, and I. Marc, "Multi-criteria optimization using a
#' genetic algorithm for determining a Pareto set," Int. J. Syst. Sci., vol. 27,
#' no. 2, pp. 255-260, 1996
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MOP7 function as a \code{smoof_multi_objective_function} object.
#' @export
makeMOP7Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    .Call("mof_MOP7", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP7 function",
    id = sprintf("MOP7-%id-%io", 2L, 3L),
    description = "MOP7 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-400, 2L),
      upper = rep(400, 2L),
      vector = TRUE
    ),
    n.objectives = 3L
  )
}

class(makeMOP7Function) = c("function", "smoof_generator")
attr(makeMOP7Function, "name") = c("MOP7")
attr(makeMOP7Function, "type") = c("multi-objective")
attr(makeMOP7Function, "tags") = c("multi-objective")
