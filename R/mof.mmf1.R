#' @title
#' MMF1 Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multimodal multiobjective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MMF1 function as a \code{smoof_multi_objective_function} object.
#' @export
makeMMF1Function = function() {

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf1(x = x)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF1 function",
    id = sprintf("MMF1-%id-%io", 2L, n.objectives),
    description = "MMF1 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(1, -1),
      upper = c(3, 1),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF1Function) = c("function", "smoof_generator")
attr(makeMMF1Function, "name") = c("MMF1")
attr(makeMMF1Function, "type") = c("multi-objective")
attr(makeMMF1Function, "tags") = c("multi-objective")
