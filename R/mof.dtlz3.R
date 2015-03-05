#' DTLZ3 function (family) generator.
#'
#' Builds and returns the multi-objective DTLZ3 test problem. The formula
#' is very similar to the formula of DTLZ2, but it uses the \eqn{g} function
#' of DTLZ1, which introduces a lot of local Pareto-optimal fronts. Thus, this
#' problems is well suited to check the ability of an optimizer to converge
#' to the global Pareto-optimal front.
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
makeDTLZ3Function = function(dimensions, n.objectives) {
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
    # the only differenc between DTLZ2 and DTLZ3
    g = 100 * (k + sum((xm - 0.5)^2 - cos(20 * pi * (xm - 0.5))))
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
    name = "DTLZ3 function",
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
