#' @title
#' MMF1e Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#'
#' @param a [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{a = exp(1L)}.
#'
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multimodal multiobjective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the MMF1e function as a \code{smoof_multi_objective_function} object.
#' @export
makeMMF1eFunction = function(a = exp(1L)) {
  checkmate::assertNumber(a, finite = TRUE)
  force(a)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_mmf1_e(x = x, a = a)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "MMF1e function",
    id = sprintf("MMF1e-%id-%io", 2L, n.objectives),
    description = "MMF1e function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(1, -(a^3)),
      upper = c(3, a^3),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeMMF1eFunction) = c("function", "smoof_generator")
attr(makeMMF1eFunction, "name") = c("MMF1e")
attr(makeMMF1eFunction, "type") = c("multi-objective")
attr(makeMMF1eFunction, "tags") = c("multi-objective")
