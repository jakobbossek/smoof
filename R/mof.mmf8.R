#' @title
#' MMF8 Function
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
makeMMF8Function = function() {
  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf8(x = x)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF8 function",
    id = sprintf("MMF8-%id-%io", 2L, n.objectives),
    description = "MMF8 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-pi, 0),
      upper = c(pi, 9),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF8Function) = c("function", "smoof_generator")
attr(makeMMF8Function, "name") = c("MMF8")
attr(makeMMF8Function, "type") = c("multi-objective")
attr(makeMMF8Function, "tags") = c("multi-objective")
