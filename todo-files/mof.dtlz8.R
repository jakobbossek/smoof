#' DTLZ8 Function (family)
#'
#' Builds and returns the multi-objective DTLZ8 test problem. This problem
#' can be characterized by a disconnected Pareto-optimal front in the search
#' space. This introduces a new challenge to evolutionary multi-objective
#' optimizers, i.e., to maintain different subpopulations within the search
#' space to cover the entire Pareto-optimal front.
#FIXME: add formula
#'
#' @references K. Deb and L. Thiele and M. Laumanns and E. Zitzler. Scalable
#' Multi-Objective Optimization Test Problems. Computer Engineering and Networks 
#' Laboratory (TIK), Swiss Federal Institute of Technology (ETH) Zurich, 112, 2001
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeDTLZ8Function = function(dimensions, n.objectives) {
  stopifnot(dimensions >= n.objectives)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  assertInt(dimensions, na.ok = FALSE, lower = 2L)
  
  # Renaming n.objectives here to stick to the notation in the paper
  M = n.objectives
  
  force(M)
  
  # TODO: C++ implementation
  # fn = function(x) {
  #   stopifnot(length(x) == dimensions)
  #   dtlz_8(x, M)
  # }
  
  fn = function(x) {
    f = numeric(M)
    n = length(x)
    g = numeric(M)
    
    z = n / M
    
    a = 1 / floor(z)
    
    for(j in 1:M) {
      f[j] = a * sum(x[floor((j - 1) * z):floor(j * z)])
    }
    
    fi = f[1:M-1]
    
    g[1:M-1] = f[M] + 4 * fi - 1
    g[M] = 2 * f[M] + sum(min(fi) + sort(fi)[2]) - 1
    
    stopifnot(g >= 0)
    
    return(f)
  }
  
  makeMultiObjectiveFunction(
    name = "DTLZ8 Function",
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

class(makeDTLZ8Function) = c("function", "smoof_generator")
attr(makeDTLZ8Function, "name") = c("DTLZ8 Function")
attr(makeDTLZ8Function, "type") = c("multi-objective")
attr(makeDTLZ8Function, "tags") = c()