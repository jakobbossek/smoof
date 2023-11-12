#' @title
#' MMF14a Function
#'
#' @description
#' Test problem from the set of "multimodal multiobjective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
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
makeMMF14aFunction = function(dimensions, n.objectives, np = 2L) {
  assertInt(n.objectives, lower = 2L)
  assertInt(dimensions, lower = n.objectives)
  assertInt(x = np, lower = 1L)
  force(np)

  # Renaming var here to stick to the notation in the paper
  M = n.objectives
  force(M)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, dimensions)
    mof_cec2019_mmf14_a(x = x, M = M, np = np)
  }

  makeMultiObjectiveFunction(
    name = "MMF14a function",
    id = sprintf("MMF14a-%id-%io", dimensions, n.objectives),
    description = "MMF14a function",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF14aFunction) = c("function", "smoof_generator")
attr(makeMMF14aFunction, "name") = c("MMF14a")
attr(makeMMF14aFunction, "type") = c("multi-objective")
attr(makeMMF14aFunction, "tags") = c("multi-objective")
