#' @title
#' MMF11 Function
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
makeMMF11Function = function(np = 2L) {
  assertInt(x = np, lower = 1L)
  force(np)

  # C implementation
  fn = function(x) {
    assertNumeric(x, len = 2L, any.missing = FALSE, all.missing = FALSE, finite = TRUE)
    return(mof_cec2019_mmf11(x = x, np = np))
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF11 function",
    id = sprintf("MMF11-%id-%io", 2L, n.objectives),
    description = "MMF11 function",
    fn = fn,
    par.set =  makeNumericParamSet(
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

class(makeMMF11Function) = c("function", "smoof_generator")
attr(makeMMF11Function, "name") = c("MMF11")
attr(makeMMF11Function, "type") = c("multi-objective")
attr(makeMMF11Function, "tags") = c("multi-objective")
