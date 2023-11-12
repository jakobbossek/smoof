#' @title
#' MOP1 function generator.
#'
#' @description
#' MOP1 function from Van Valedhuizen's test suite.
#'
#' @references
#' J. D. Schaffer, "Multiple objective optimization with vector evaluated
#' genetic algorithms," in Proc. 1st Int. Conf. Genetic Algorithms and Their
#' Applications, J. J. Grenfenstett, Ed., 1985, pp. 93-100.
#'
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeMOP1Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    .Call("mof_MOP1", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP1 function",
    id = "MOP1",
    description = "MOP1 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = -1e5,
      upper = 1e5,
      vector = TRUE
      ),
    n.objectives = 2L#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeMOP1Function) = c("function", "smoof_generator")
attr(makeMOP1Function, "name") = c("MOP1")
attr(makeMOP1Function, "type") = c("multi-objective")
attr(makeMOP1Function, "tags") = c("multi-objective")
