#' DTLZ2 function (family) generator.
#'
#' Builds and returns the multi-objective DTLZ2 test problem.
#FIXME: add formula
#'
#' @references K. Deb and L. Thiele and M. Laumanns and E. Zitzler. Scalable
#' Multi-Objective Optimization Test Problems.
#'
#FIXME: must this be greater than n.objectives?
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeDTLZ2Function = function(dimensions, n.objectives) {
  # Renaming vars here to stick to the notation in the paper
  # number of decision variables in the last group (see x_m in the paper)
  k = dimensions - n.objectives + 1
  M = n.objectives

  force(M)
  force(k)

  fn = function(x) {
    #FIXME: check that implementation
    #FIXME: maybe implement this stuff in C++. Yes, the benchmarks for the
    # mono-objective test function were not that good, but maybe
    f = numeric(M)
    n = length(x)
    xm = x[(n - k):n]
    g = sum((xm - 0.5)^2)
    a = (1 + g)
    prod.xi = 1
    for(i in M:2) {
      f[i] = a * prod.xi * sin(x[M - i + 1] * pi * 0.5)
      prod.xi = prod.xi * cos(x[M - i + 1] * pi * 0.5)
    }
    f[1] = a * prod.xi
    return(f)
  }

  makeMultiObjectiveFunction(
    name = "DTLZ2 function",
    description = "Deb et al.",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = FALSE
      ),
    n.objectives = n.objectives
  )
}
