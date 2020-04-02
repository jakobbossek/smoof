#' @title
#' MMF3 Function
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
makeMMF3Function = function() {
  # C implementation
  fn = function(x) {
    assertNumeric(x, len = 2L, any.missing = FALSE, all.missing = FALSE, finite = TRUE)
    return(mof_cec2019_mmf3(x = x))
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF3 function",
    id = sprintf("MMF3-%id-%io", 2L, n.objectives),
    description = "MMF3 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(0, 2),
      upper = c(1, 1.5),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF3Function) = c("function", "smoof_generator")
attr(makeMMF3Function, "name") = c("MMF3")
attr(makeMMF3Function, "type") = c("multi-objective")
attr(makeMMF3Function, "tags") = c("multi-objective")
