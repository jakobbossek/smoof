#' @title
#' MMF9 Function
#'
#' @description
#' Test problem from the set of "multimodal multiobjective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @param np [\code{integer}(1)]\cr
#'   Number of global Pareto sets. In the CEC2019 competition, the organizers used
#'   \code{np = 2L}.
#'
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multimodal multiobjective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeMMF9Function = function(np = 2L) {
  assertInt(x = np, lower = 1L)
  force(np)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf9(x = x, np = np)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF9 function",
    id = sprintf("MMF9-%id-%io", 2L, n.objectives),
    description = "MMF9 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(0.1, 2),
      upper = rep(1.1, 2),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF9Function) = c("function", "smoof_generator")
attr(makeMMF9Function, "name") = c("MMF9")
attr(makeMMF9Function, "type") = c("multi-objective")
attr(makeMMF9Function, "tags") = c("multi-objective")
