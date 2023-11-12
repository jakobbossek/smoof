#' @title
#' MOP2 function generator.
#'
#' @description
#' MOP2 function from Van Valedhuizen's test suite due to Fonseca and Fleming.
#'
#' @references
#' C. M. Fonseca and P. J. Fleming, "Multiobjective genetic algorithms
#' made easy: Selection, sharing and mating restriction," Genetic Algorithms
#' in Engineering Systems: Innovations and Applications, pp. 45-52, Sep. 1995. IEE.
#'
#' @template arg_dimensions
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeMOP2Function = function(dimensions = 2L) {
  assertInt(dimensions, lower = 1L)
  force(dimensions)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, dimensions)
    .Call("mof_MOP2", x)
  }

  makeMultiObjectiveFunction(
    name = "MOP2 function",
    id = sprintf("MOP2-%id-%io", dimensions, 2L),
    description = "MOP2 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-4, dimensions),
      upper = rep(4, dimensions),
      vector = TRUE
    ),
    n.objectives = 2L#,
    #ref.point = rep(11, n.objectives)
  )
}

class(makeMOP2Function) = c("function", "smoof_generator")
attr(makeMOP2Function, "name") = c("MOP2")
attr(makeMOP2Function, "type") = c("multi-objective")
attr(makeMOP2Function, "tags") = c("multi-objective")
