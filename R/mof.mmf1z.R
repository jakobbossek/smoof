#' @title
#' MMF1z Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#'
#' @param k [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{k = 3}.
#'
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multi-modal multi-objective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#' Returns an instance of the MMF1z function as a \code{smoof_multi_objective_function} object.
#' 
#' @export
makeMMF1zFunction = function(k = 3) {
  checkmate::assertNumber(k, finite = TRUE)
  force(k)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf1_z(x = x, k = k)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF1z function",
    id = sprintf("MMF1z-%id-%io", 2L, n.objectives),
    description = "MMF1z function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(1, -1),
      upper = c(3, 1),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF1zFunction) = c("function", "smoof_generator")
attr(makeMMF1zFunction, "name") = c("MMF1z")
attr(makeMMF1zFunction, "type") = c("multi-objective")
attr(makeMMF1zFunction, "tags") = c("multi-objective")
