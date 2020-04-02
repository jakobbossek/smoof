#' @title
#' MMF15a Function
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
makeMMF15aFunction = function(dimensions, n.objectives, np = 2L) {
  assertInt(n.objectives, lower = 2L)
  assertInt(dimensions, lower = n.objectives)
  assertInt(x = np, lower = 1L)
  force(np)

  # Renaming var here to stick to the notation in the paper
  M = n.objectives
  force(M)

  # C implementation
  fn = function(x) {
    assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE, finite = TRUE)
    return(mof_cec2019_mmf15_a(x = x, M = M, np = np))
  }

  makeMultiObjectiveFunction(
    name = "MMF15a function",
    id = sprintf("MMF15a-%id-%io", dimensions, n.objectives),
    description = "MMF15a function",
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

class(makeMMF13Function) = c("function", "smoof_generator")
attr(makeMMF13Function, "name") = c("MMF15a")
attr(makeMMF13Function, "type") = c("multi-objective")
attr(makeMMF13Function, "tags") = c("multi-objective")
