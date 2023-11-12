#' @title
#' MMF5 Function
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
makeMMF5Function = function() {
  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf5(x = x)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF5 function",
    id = sprintf("MMF5-%id-%io", 2L, n.objectives),
    description = "MMF5 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(1, -1),
      upper = c(3, 3),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF5Function) = c("function", "smoof_generator")
attr(makeMMF5Function, "name") = c("MMF5")
attr(makeMMF5Function, "type") = c("multi-objective")
attr(makeMMF5Function, "tags") = c("multi-objective")
