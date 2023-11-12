#' @title
#' MMF13 Function
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
#' 
#' @export
makeMMF13Function = function(np = 2L) {
  assertInt(x = np, lower = 1L)
  force(np)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 3L)
    mof_cec2019_mmf13(x = x, np = np)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF13 function",
    id = sprintf("MMF13-%id-%io", 3L, n.objectives),
    description = "MMF13 function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = 3L,
      id = "x",
      lower = rep(0.1, 3L),
      upper = rep(1.1, 3L),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF13Function) = c("function", "smoof_generator")
attr(makeMMF13Function, "name") = c("MMF13")
attr(makeMMF13Function, "type") = c("multi-objective")
attr(makeMMF13Function, "tags") = c("multi-objective")
