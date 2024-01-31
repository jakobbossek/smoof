#' @title
#' MMF10 Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multi-modal multi-objective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#' Returns an instance of the MMF10 function as a \code{smoof_multi_objective_function} object.
#' 
#' @export
makeMMF10Function = function() {
  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf10(x = x)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF10 function",
    id = sprintf("MMF10-%id-%io", 2L, n.objectives),
    description = "MMF10 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(0.1, 2L),
      upper = rep(1.1, 2L),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF10Function) = c("function", "smoof_generator")
attr(makeMMF10Function, "name") = c("MMF10")
attr(makeMMF10Function, "type") = c("multi-objective")
attr(makeMMF10Function, "tags") = c("multi-objective")
