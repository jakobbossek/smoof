#' DTLZ2 Function (family)
#'
#' Builds and returns the multi-objective DTLZ2 test problem.
#' \deqn{\text{Minimize } &\; f_1(\textbf{x}) = (1+g(\textbf{x}_M)) \cos(x_1\pi/2) \cos(x_2\pi/2) \cdots \cos(x_{M-2}\pi/2) \cos(x_{M-1}\pi/2),\\
#' \text{Minimize } &\; f_2(\textbf{x}) = (1+g(\textbf{x}_M)) \cos(x_1\pi/2) \cos(x_2\pi/2) \cdots \cos(x_{M-2}\pi/2) \sin(x_{M-1}\pi/2),\\
#' \text{Minimize } &\; f_3(\textbf{x}) = (1+g(\textbf{x}_M)) \cos(x_1\pi/2) \cos(x_2\pi/2) \cdots \sin(x_{M-2}\pi/2),\\
#' \vdots &\; \vdots\\
#' \text{Minimize } &\; f_{M-1}(\textbf{x}) = (1+g(\textbf{x}_M)) \cos(x_1\pi/2) \sin(x_2\pi/2),\\
#' \text{Minimize } &\; f_{M}(\textbf{x}) = (1+g(\textbf{x}_M)) \sin(x_1\pi/2),\\
#' &\; 0 \leq x_i \leq 1, \text{ for } i=1,2,\dots,n,\\
#' \text{where} &\; g(\textbf{x}_M) = \sum\limits_{x_i\in\textbf{x}_M}(x_i-0.5)^2.}
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
makeDTLZ2Function = function(dimensions, n.objectives) {
  stopifnot(dimensions >= n.objectives)
  
  # Renaming n.objectives here to stick to the notation in the paper
  M = n.objectives

  force(M)
  
  # C++ implementation
  fn = function(x) {
    stopifnot(length(x) == dimensions)
    dtlz_2(x, M)
  }

  # fn = function(x) {
  #   stopifnot(length(x) == dimensions)
  #   f = numeric(M)
  #   n = length(x)
  #   xm = x[M:n]
  #   g = sum((xm - 0.5)^2)
  #   a = (1 + g)
  #   prod.xi = 1
  #   for(i in M:2) {
  #     f[i] = a * prod.xi * sin(x[M - i + 1] * pi * 0.5)
  #     prod.xi = prod.xi * cos(x[M - i + 1] * pi * 0.5)
  #   }
  #   f[1] = a * prod.xi
  #   return(f)
  # }

  makeMultiObjectiveFunction(
    name = "DTLZ2 Function",
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

class(makeDTLZ2Function) = c("function", "smoof_generator")
attr(makeDTLZ2Function, "name") = c("DTLZ2 Function")
attr(makeDTLZ2Function, "type") = c("multi-objective")
attr(makeDTLZ2Function, "tags") = c()
