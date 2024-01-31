#' @title
#' SYM-PART Rotated Function
#'
#' @description
#' Test problem from the set of "multi-modal multi-objective functions" as for
#' instance used in the CEC2019 competition.
#' 
#' @param w [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{w = pi / 4}.
#' @param a [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{a = 1}.
#' @param b [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{b = 10}.
#' @param c [\code{double}(1)]\cr
#'   Parametrizable factor. In the CEC2019 competition, the organizers used
#'   \code{c = 8}.
#'
#' @references
#' Caitong Yue, Boyang Qu, Kunjie Yu, Jing Liang, and Xiaodong Li, "A novel
#' scalable test problem suite for multi-modal multi-objective optimization," in
#' Swarm and Evolutionary Computation, Volume 48, August 2019, pp. 62–71, Elsevier.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the SYM-PART Rotated function as a \code{smoof_multi_objective_function} object.
#'  
#' @export
makeSYMPARTrotatedFunction = function(w = pi / 4, a = 1, b = 10, c = 8) {
  checkmate::assertNumber(x = w, finite = TRUE)
  checkmate::assertNumber(x = a, finite = TRUE)
  checkmate::assertNumber(x = b, finite = TRUE)
  checkmate::assertNumber(x = c, finite = TRUE)
  force(w)
  force(a)
  force(b)
  force(c)

  # C implementation
  fn = function(x) {
    checkNumericInput(x, 2L)
    mof_cec2019_SYM_PART_ROTATED(x = x, w = w, a = a, b = b, c = c)
  }

  n.objectives = 2L
  makeMultiObjectiveFunction(
    name = "SYMPART-rotated function",
    id = sprintf("SYMPART-rotated-%id-%io", 2L, n.objectives),
    description = "SYMPART-rotated function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-20, 2L),
      upper = rep(20, 2L),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeSYMPARTrotatedFunction) = c("function", "smoof_generator")
attr(makeSYMPARTrotatedFunction, "name") = c("SYMPART-rotated")
attr(makeSYMPARTrotatedFunction, "type") = c("multi-objective")
attr(makeSYMPARTrotatedFunction, "tags") = c("multi-objective")
