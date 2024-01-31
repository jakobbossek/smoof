#' @title
#' ED2 Function
#'
#' @description
#' Builds and returns the multi-objective ED2 test problem.
#'
#' The ED2 test problem is defined as follows:
#'
#' Minimize \eqn{f_j(\mathbf{x}) = \frac{1}{F_{natmin}(\mathbf{x}) + 1} \cdot \tilde{p}(\Theta (\mathbf{X}))}{
#' f[j](X) = (1 / (F[natmin](X) + 1)) * p(\Theta(X))}, for \eqn{j = 1, \ldots, m}{j = 1, ..., m},
#'
#' with \eqn{\mathbf{x} = (x_1, \ldots, x_n)^T}{X = (x[1], ..., x[n])}, where \eqn{0 \leq x_i \leq 1}{0 \le x[i] \le 1},
#' and \eqn{\Theta = (\theta_1, \ldots, \theta_{m-1})}{\Theta = (\theta[1], ..., \theta[m-1])},
#' where \eqn{0 \le \theta_j \le \frac{\pi}{2}}{0 \le \theta[j] \le \pi/2}, for \eqn{i = 1, \ldots, n,}{i = 1, ..., n} and \eqn{j = 1, \ldots, m - 1}{j = 1, ..., m - 1}.
#'
#'
#' Moreover \eqn{F_{natmin}(\mathbf{x}) = b + (r(\mathbf{x}) - a) + 0.5 + 0.5 \cdot (2 \pi \cdot (r(\mathbf{x}) - a) + \pi)}{F[natmin](X) = b + (r(X) - a) + 0.5 + 0.5 * (2 \pi * (r(X) - a) + \pi)}
#' 
#' with \eqn{a \approx 0.051373}{a = 0.051373}, \eqn{b \approx 0.0253235}{b = 0.0253235}, and \eqn{r(\mathbf{X}) = \sqrt{x_m^2 + \ldots, x_n^2}}{r(X) = sqrt(x[m]^2 + ... + x[n]^2)}, as well as
#' 
#' \eqn{\tilde{p}_1(\Theta) = \cos(\theta_1)^{2/\gamma}}{p[1](\Theta) = cos(\theta[1])^(2/\gamma)},
#' 
#' \eqn{\tilde{p}_j(\Theta) = \left( \sin(\theta_1) \cdot \ldots \cdot \sin(\theta_{j - 1}) \cdot \cos(\theta_j) \right)^{2/\gamma}}{p[j](\Theta) = (sin(\theta[1]) * ... * sin(\theta[j - 1]) * cos(\theta[j]))^(2/\gamma)},
#' for \eqn{2 \le j \le m - 1}{2 \le j \le m - 1},
#' 
#' and \eqn{\tilde{p}_m(\Theta) = \left( \sin(\theta_1) \cdot \ldots \cdot \sin(\theta_{m - 1}) \right)^{2/\gamma}}{p[m](\Theta) = (sin(\theta[1]) * ... * sin(\theta[m - 1]))^(2/\gamma)}.

#'
#' @references M. T. M. Emmerich and A. H. Deutz. Test Problems based on Lame
#' Superspheres. Proceedings of the International Conference on Evolutionary
#' Multi-Criterion Optimization (EMO 2007), pp. 922-936, Springer, 2007.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives.
#' @param gamma [\code{numeric(1)}]\cr
#'   Optional parameter. Default is 2, which is recommended by Emmerich and Deutz.
#' @param theta [\code{numeric(dimensions)}]\cr
#'   Parameter vector, whose components have to be between \code{0} and \code{0.5*pi}.
#'   The default is \code{theta = (pi/2) * x} (with \code{x} being the point from the decision space) as recommended by Emmerich and Deutz.
#' @return [\code{smoof_multi_objective_function}]
#'  Returns an instance of the ED2 function as a \code{smoof_multi_objective_function} object.
#' @export
makeED2Function = function(dimensions, n.objectives, gamma = 2, theta) {
  checkmate::assertInt(n.objectives, lower = 2L)
  checkmate::assertInt(dimensions, lower = n.objectives)
  checkmate::assertNumber(gamma, na.ok = FALSE, lower = .Machine$double.eps)

  force(n.objectives)
  force(dimensions)
  force(gamma)

  if (missing(theta)) {
    theta = expression((pi / 2) * x)
  }

  # C++ implementation
  fn = function(x) {
    checkNumericInput(x, dimensions, lower = 0, upper = 1)
    theta = eval(theta, list(x))
    checkNumericInput(theta, dimensions, lower = 0, upper = pi/2)
    force(theta)
    mof_ED_2(x, n.objectives, gamma, theta)
  }

  makeMultiObjectiveFunction(
    name = "ED2 Function",
    id = paste0("ED2_", dimensions, "d_", n.objectives, "o"),
    description = "Emmerich and Deutz",
    fn = fn,
    par.set =  ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
      ),
    n.objectives = n.objectives,
    ref.point = rep(1, n.objectives)
  )
}

class(makeED2Function) = c("function", "smoof_generator")
attr(makeED2Function, "name") = c("ED2")
attr(makeED2Function, "type") = c("multi-objective")
attr(makeED2Function, "tags") = c("multi-objective")
