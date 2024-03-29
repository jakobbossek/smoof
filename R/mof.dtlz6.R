#' @title
#' DTLZ6 Function (family)
#'
#' @description
#' Builds and returns the multi-objective DTLZ6 test problem. This problem
#' can be characterized by a disconnected Pareto-optimal front in the search
#' space. This introduces a new challenge to evolutionary multi-objective
#' optimizers, i.e., to maintain different subpopulations within the search
#' space to cover the entire Pareto-optimal front.
#'
#' The DTLZ6 test problem is defined as follows:
#'
#' Minimize \eqn{f_1(\mathbf{x}) = (1+g(\mathbf{x}_M)) \cos(\theta_1\pi/2) \cos(\theta_2\pi/2) \cdots \cos(\theta_{M-2}\pi/2) \cos(\theta_{M-1}\pi/2),}{
#' f[1](X) = (1 + g(XM)) * cos(theta[1] * pi/2) * cos(theta[2] * pi/2) * ... * cos(theta[M-2] * pi/2) * cos(theta[M-1] * pi/2)}
#'
#' Minimize \eqn{f_2(\mathbf{x}) = (1+g(\mathbf{x}_M)) \cos(\theta_1\pi/2) \cos(\theta_2\pi/2) \cdots \cos(\theta_{M-2}\pi/2) \sin(\theta_{M-1}\pi/2),}{
#' f[2](X) = (1 + g(XM)) * cos(theta[1] * pi/2) * cos(theta[2] * pi/2) * ... * cos(theta[M-2] * pi/2) * sin(theta[M-1] * pi/2)}
#'
#' Minimize \eqn{f_3(\mathbf{x}) = (1+g(\mathbf{x}_M)) \cos(\theta_1\pi/2) \cos(\theta_2\pi/2) \cdots \sin(\theta_{M-2}\pi/2),}{
#' f[3](X) = (1 + g(XM)) * cos(theta[1] * pi/2) * cos(theta[2] * pi/2) * ... * sin(theta[M-2] * pi/2)}
#'
#' \eqn{\vdots\\}{...}
#'
#' Minimize \eqn{f_{M-1}(\mathbf{x}) = (1+g(\mathbf{x}_M)) \cos(\theta_1\pi/2) \sin(\theta_2\pi/2),}{
#' f[M-1](X) = (1 + g(XM)) * cos(theta[1] * pi/2) * sin(theta[2] * pi/2)}
#'
#' Minimize \eqn{f_{M}((1+g(\mathbf{x}_M)) \sin(\theta_1\pi/2),}{
#' f[M](X) = (1 + g(XM)) * sin(theta[1] * pi/2)}
#'
#' with \eqn{0 \leq x_i \leq 1}{0 <= x[i] <= 1}, for \eqn{i=1,2,\dots,n,}{i=1,2,...,n}
#'
#' where \eqn{\theta_i = \frac{\pi}{4(1+ g(\mathbf{x}_M))} (1+2g(\mathbf{x}_M)x_i),}{
#' theta[i] = pi / (4 * (1 + g(XM))) * (1 + 2 * g(XM) * x[i]),}
#' for \eqn{i = 2,3,\dots,(M-1)}{i = 2,3,...,(M-1)}
#'
#' and \eqn{g(\mathbf{x}_M) = \sum\limits_{x_i\in\mathbf{x}_M}x_i^{0.1}}{
#' g(XM) = sum{x[i] in XM} {x[i]^0.1}}
#'
#' @references K. Deb and L. Thiele and M. Laumanns and E. Zitzler. Scalable
#' Multi-Objective Optimization Test Problems. Computer Engineering and Networks
#' Laboratory (TIK), Swiss Federal Institute of Technology (ETH) Zurich, 112, 2001
#'
#' @note Attention: Within the succeeding work of Deb et al. (K. Deb and L. Thiele and
#' M. Laumanns and E. Zitzler (2002). Scalable multi-objective optimization test problems,
#' Proceedings of the IEEE Congress on Evolutionary Computation, pp. 825-830)
#' this problem was called DTLZ5.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the DTLZ6 family as a \code{smoof_multi_objective_function} object.
#' @export
makeDTLZ6Function = function(dimensions, n.objectives) {
  checkmate::assertInt(n.objectives, lower = 2L)
  checkmate::assertInt(dimensions, lower = n.objectives)

  # Renaming n.objectives here to stick to the notation in the paper
  M = n.objectives

  force(M)

  # C++ implementation
  fn = function(x) {
    checkNumericInput(x, dimensions)
    dtlz_6(x, M)
  }

  # Reference R implementation
  # fn = function(x) {
  #   assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
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
    name = "DTLZ6 Function",
    id = paste0("dtlz6_", dimensions, "d_", n.objectives, "o"),
    description = "Deb et al.",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
      ),
    n.objectives = n.objectives,
    ref.point = rep(11, n.objectives)
  )
}

class(makeDTLZ6Function) = c("function", "smoof_generator")
attr(makeDTLZ6Function, "name") = c("DTLZ6")
attr(makeDTLZ6Function, "type") = c("multi-objective")
attr(makeDTLZ6Function, "tags") = c("multi-objective")
