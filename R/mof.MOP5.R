#' @title
#' MOP5 function generator.
#'
#' @description
#' MOP5 function from Van Valedhuizen's test suite.
#'
#' @note
#' Original box constraints where \eqn{[-3, 3]}.
#'
#' @references
#' R. Viennet, C. Fonteix, and I. Marc, "Multi criteria optimization using a
#' genetic algorithm for determining a Pareto set," Int. J. Syst. Sci., vol. 27,
#' no. 2, pp. 255-260, 1996
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MOP5 function as a \code{smoof_multi_objective_function} object.
#' @export
makeMOP5Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    .Call("mof_MOP5", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP5 function",
    id = sprintf("MOP5-%id-%io", 2L, 3L),
    description = "MOP5 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-30, 2L),
      upper = rep(30, 2L),
      vector = TRUE
    ),
    n.objectives = 3L
  )
}

class(makeMOP5Function) = c("function", "smoof_generator")
attr(makeMOP5Function, "name") = c("MOP5")
attr(makeMOP5Function, "type") = c("multi-objective")
attr(makeMOP5Function, "tags") = c("multi-objective")
