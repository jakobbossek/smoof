#' @title
#' MMF4 Function
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
makeMMF4Function = function() {
  # C implementation
  fn = function(x) {
    assertNumeric(x, len = 2L, any.missing = FALSE, all.missing = FALSE, finite = TRUE)
    return(mof_cec2019_mmf4(x = x))
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF4 function",
    id = sprintf("MMF4-%id-%io", 2L, n.objectives),
    description = "MMF4 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-1, 0),
      upper = c(1, 2),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF4Function) = c("function", "smoof_generator")
attr(makeMMF4Function, "name") = c("MMF4")
attr(makeMMF4Function, "type") = c("multi-objective")
attr(makeMMF4Function, "tags") = c("multi-objective")
