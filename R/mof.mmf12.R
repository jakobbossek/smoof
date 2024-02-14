#' @title
#' MMF12 Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @param np [\code{integer}(1)]\cr
#'   Number of global Pareto sets. In the CEC2019 competition, the organizers used
#'   \code{np = 2L}.
#' @param q [\code{integer}(1)]\cr
#'   Number of discontinuous pieces in each Pareto front. In the CEC2019 competition,
#'   the organizers used \code{q = 4L}.
#'
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multi-modal multi-objective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#' Returns an instance of the MMF12 function as a \code{smoof_multi_objective_function} object.
#' 
#' @export
makeMMF12Function = function(np = 2L, q = 4L) {
  checkmate::assertInt(x = np, lower = 1L)
  checkmate::assertInt(x = q, lower = 1L)
  force(np)
  force(q)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    return(mof_cec2019_mmf12(x = x, np = np, q = q))
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF12 function",
    id = sprintf("MMF12-%id-%io", 2L, n.objectives),
    description = "MMF12 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(0, 2L),
      upper = rep(1, 2L),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF12Function) = c("function", "smoof_generator")
attr(makeMMF12Function, "name") = c("MMF12")
attr(makeMMF12Function, "type") = c("multi-objective")
attr(makeMMF12Function, "tags") = c("multi-objective")
