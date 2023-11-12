#' @title
#' MMF6 Function
#'
#' @description
#' Test problem from the set of "multimodal multiobjective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multimodal multiobjective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeMMF6Function = function() {
  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf6(x = x)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF6 function",
    id = sprintf("MMF6-%id-%io", 2L, n.objectives),
    description = "MMF6 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(1, -1),
      upper = c(3, 2),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF6Function) = c("function", "smoof_generator")
attr(makeMMF6Function, "name") = c("MMF6")
attr(makeMMF6Function, "type") = c("multi-objective")
attr(makeMMF6Function, "tags") = c("multi-objective")
