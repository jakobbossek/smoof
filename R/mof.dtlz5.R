#' DTLZ5 Function (family)
#'
#' Builds and returns the multi-objective DTLZ5 test problem. This problem
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
makeDTLZ5Function = function(dimensions, n.objectives) {
  stopifnot(dimensions >= n.objectives)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  
  # Renaming n.objectives here to stick to the notation in the paper
  M = n.objectives
  
  force(M)
  
  # C++ implementation
  fn = function(x) {
    stopifnot(length(x) == dimensions)
    dtlz_5(x, M)
  }
  
  # fn = function(x) {
  #   stopifnot(length(x) == dimensions)
  #   f = numeric(M)
  #   n = length(x)
  #   theta = numeric(M-1)
  #   xm = x[M:n]
  #   g = sum(xm^0.1)
  #   t = pi / (4 * (1 + g))
  #   theta[1] = x[1] * pi / 2
  #   theta[-1] = t * (1 + 2 * g * x[2:(M - 1)])
  #   a = (1 + g)
  #   prod.xi = 1
  #   for(i in M:2) {
  #     f[i] = a * prod.xi * sin(theta[M - i + 1])
  #     prod.xi = prod.xi * cos(theta[M - i + 1])
  #   }
  #   f[1] = a * prod.xi
  #   return(f)
  # }
  
  makeMultiObjectiveFunction(
    name = "DTLZ5 Function",
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

class(makeDTLZ5Function) = c("function", "smoof_generator")
attr(makeDTLZ5Function, "name") = c("DTLZ5 Function")
attr(makeDTLZ5Function, "type") = c("multi-objective")
attr(makeDTLZ5Function, "tags") = c()
