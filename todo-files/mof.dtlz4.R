#' DTLZ4 Function (family)
#'
#' Builds and returns the multi-objective DTLZ4 test problem. It is a slight
#' modification of the DTLZ2 problems by introducing the parameter \eqn{\alpha}.
#' The parameter is used to map \eqn{\mathbf{x}_i \rightarrow \mathbf{x}_i^{\alpha}}.
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
#' @param alpha [\code{numeric(1)}]\cr
#'   Optional parameter. Default is 100, which is recommended by Deb et al.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeDTLZ4Function = function(dimensions, n.objectives, alpha = 100) {
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  assertNumber(alpha, na.ok = FALSE)

  # Renaming vars here to stick to the notation in the paper
  # number of decision variables in the last group (see x_m in the paper)
  k = dimensions - n.objectives + 1
  M = n.objectives

  force(M)
  force(k)
  force(alpha)

  fn = function(x) {
    #FIXME: check that implementation
    #FIXME: maybe implement this stuff in C++. Yes, the benchmarks for the
    # mono-objective test function were not that good, but maybe
    f = numeric(M)
    n = length(x)
    # only difference to DTLZ
    x = x^alpha
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
    name = "DTLZ4 Function",
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

class(makeDTLZ4Function) = c("function", "smoof_generator")
attr(makeDTLZ4Function, "name") = c("DTLZ4 Function")
attr(makeDTLZ4Function, "type") = c("multi-objective")
attr(makeDTLZ4Function, "tags") = c()
