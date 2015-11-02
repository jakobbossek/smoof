#' DTLZ1 Function (family)
#'
#' Builds and returns the multi-objective DTLZ1 test problem.
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
makeDTLZ1Function = function(dimensions, n.objectives) {
  stopifnot(dimensions >= n.objectives)
  
  # Renaming vars here to stick to the notation in the paper
  # number of decision variables in the last group (see x_m in the paper)
  k = dimensions - n.objectives + 1
  M = n.objectives

  force(M)
  force(k)
  
  # C++ implementation
  fn = function(x) {
    stopifnot(length(x) == dimensions)
    dtlz_1(x, M)
  }

  # fn = function(x) {
  #   stopifnot(length(x) == dimensions)
  #   f = numeric(M)
  #   n = length(x)
  #   xm = x[M:n]
  #   g = 100 * (k + sum((xm - 0.5)^2 - cos(20 * pi * (xm - 0.5))))
  #   a = 0.5 * (1 + g)
  #   prod.xi = 1
  #   for(i in M:2) {
  #     f[i] = a * prod.xi * (1 - x[M - i + 1])
  #     prod.xi = prod.xi * x[M - i + 1]
  #   }
  #   f[1] = a * prod.xi
  #   return(f)
  # }

  makeMultiObjectiveFunction(
    name = "DTLZ1 Function",
    description = "Deb et al.",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
            #FIXME: any box constraints?
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
      ),
    n.objectives = n.objectives
  )
}

class(makeDTLZ1Function) = c("function", "smoof_generator")
attr(makeDTLZ1Function, "name") = c("DTLZ1 Function")
attr(makeDTLZ1Function, "type") = c("multi-objective")
attr(makeDTLZ1Function, "tags") = c()
