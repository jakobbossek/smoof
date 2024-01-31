#' @title
#' MOP6 function generator.
#'
#' @description
#' MOP6 function from Van Valedhuizen's test suite.
#'
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MOP6 function as a \code{smoof_multi_objective_function} object.
#' @export
makeMOP6Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    .Call("mof_MOP6", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP6 function",
    id = sprintf("MOP6-%id-%io", 2L, 2L),
    description = "MOP6 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(0, 2L),
      upper = rep(1, 2L),
      vector = TRUE
    ),
    n.objectives = 2L
  )
}

class(makeMOP6Function) = c("function", "smoof_generator")
attr(makeMOP6Function, "name") = c("MOP6")
attr(makeMOP6Function, "type") = c("multi-objective")
attr(makeMOP6Function, "tags") = c("multi-objective")
