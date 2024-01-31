#' @title
#' WFG5 Function
#'
#' @description
#' Fifth test problem from the "Walking Fish Group" problem generator toolkit.
#'
#' @references
#' S. Huband, P. Hingston, L. Barone, and L. While, "A Review of Multi-objective
#' Test Problems and a Scalable Test Problem Toolkit," in IEEE Transactions on
#' Evolutionary Computation, Volume 10, No 5, October 2006, pp. 477-506. IEEE.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @param k [\code{integer}(1)]\cr
#'   Number of position-related parameters. These will automatically be the
#'   first \code{k} elements from the input vector. This value has to be a
#'   multiple of \code{n.objectives - 1}.
#' @param l [\code{integer}(1)]\cr
#'   Number of distance-related parameters. These will automatically be
#'   the last \code{l} elements from the input vector.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the WFG5 function as a \code{smoof_multi_objective_function} object.
#' @details 
#'   Huband et al. recommend a value of \code{k = 4L} position-related
#'   parameters for bi-objective problems and \code{k = 2L * (n.objectives - 1L)}
#'   for many-objective problems. Furthermore the authors recommend a value of
#'   \code{l = 20} distance-related parameters. Therefore, if \code{k} and/or
#'   \code{l} are not explicitly defined by the user, their values will be set
#'   to the recommended values per default.
#' 
#' @export
makeWFG5Function = function(n.objectives, k, l) {
  checkmate::assertInt(n.objectives, lower = 2L)
  force(n.objectives)

  if (missing(k)) {
    if (n.objectives == 2L) {
      k = 4L
    } else {
      k = 2L * (n.objectives - 1L)
    }
  }
  checkmate::assertInt(k, lower = n.objectives - 1L)
  checkmate::assertTRUE(k %% (n.objectives - 1L) == 0L)
  force(k)

  if (missing(l)) {
    l = 20L
  }
  checkmate::assertInt(l, lower = 1L)
  force(l)
  dimensions = k + l

  # C implementation
  fn = function(x) {
    checkNumericInput(x, k + l)
    mof_WFG_5(z = x, M = n.objectives, k = k)
  }

  makeMultiObjectiveFunction(
    name = "WFG5 function",
    id = sprintf("WFG5-%id-%io", dimensions, n.objectives),
    description = "WFG5 function",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = 2L * seq_len(dimensions),
      vector = TRUE
    ),
    minimize = rep(TRUE, n.objectives),
    n.objectives = n.objectives
  )
}

class(makeWFG5Function) = c("function", "smoof_generator")
attr(makeWFG5Function, "name") = c("WFG5")
attr(makeWFG5Function, "type") = c("multi-objective")
attr(makeWFG5Function, "tags") = c("multi-objective")
