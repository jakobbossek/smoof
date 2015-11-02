#' DTLZ6 Function (family)
#'
#' Builds and returns the multi-objective DTLZ6 test problem. This problem
#' can be characterized by a disconnected Pareto-optimal front in the search
#' space. This introduces a new challenge to evolutionary multi-objective
#' optimizers, i.e., to maintain different subpopulations within the search
#' space to cover the entire Pareto-optimal front.
#FIXME: add formula
#'
#' @references K. Deb and L. Thiele and M. Laumanns and E. Zitzler. Scalable
#' Multi-Objective Optimization Test Problems.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeDTLZ6Function = function(dimensions, n.objectives) {
  stopifnot(dimensions >= n.objectives)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)

  # Renaming n.objectives here to stick to the notation in the paper
  M = n.objectives

  force(M)
  
  # C++ implementation
  fn = function(x) {
    stopifnot(length(x) == dimensions)
    dtlz_6(x, M)
  }
  
  # fn = function(x) {
  #   f = numeric(M)
  #   n = length(x)
  #   f[1:(M - 1)] = x[1:(M - 1)]
  #   xm = x[(n - k):n]
  #   g = 1 + 9 * sum(xm) / k
  #   fi = f[1:(M - 1)]
  #   h = M - sum(fi  * (1 + sin(3 * pi * fi)) / (1 + g))
  #   f[M] = (1 + g) * h
  #   return(f)
  # }

  makeMultiObjectiveFunction(
    name = "DTLZ6 Function",
    description = "Deb et al.",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
      ),
    n.objectives = n.objectives
  )
}

class(makeDTLZ6Function) = c("function", "smoof_generator")
attr(makeDTLZ6Function, "name") = c("DTLZ6 Function")
attr(makeDTLZ6Function, "type") = c("multi-objective")
attr(makeDTLZ6Function, "tags") = c()
