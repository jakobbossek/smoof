#' Rastrigin Function
#'
#' A modified version of the Rastrigin function following the formula:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} 10\left(1 + \cos(2\pi k_i \mathbf{x}_i)\right) + 2 k_i \mathbf{x}_i^2.}
#' The box-constraints are given by \eqn{\mathbf{x}_i \in [0, 1]} for
#' \eqn{i = 1, \ldots, n} and \eqn{k} is a numerical vector. Deb et al. (see references)
#' use, e.g., \eqn{k = (2, 2, 3, 4)} for \eqn{n = 4}. See the reference for details.
#'
#' @references Kalyanmoy Deb and Amit Saha. Multimodal optimization using a bi-
#' objective evolutionary algorithm. Evolutionary Computation, 20(1):27-62, 2012.
#'
#' @template arg_dimensions
#' @param k [numeric]\cr
#'   Vector of numerical values of length \code{dimensions}.
#'   Default is \code{rep(1, dimensions)}
#' @template ret_smoof_single
#' @export
makeModifiedRastriginFunction = function(dimensions, k = rep(1, dimensions)) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = sprintf("%i-d Modified Rastrigin Function", dimensions),
    id = sprintf("rastrigin_%id", dimensions),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      sum(10 * (1 + cos(2 * pi * k * x)) + 2 * k * x^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    tags = attr(makeModifiedRastriginFunction, "tags")
  )
}

class(makeModifiedRastriginFunction) = c("function", "smoof_generator")
attr(makeModifiedRastriginFunction, "name") = c("Modified Rastrigin")
attr(makeModifiedRastriginFunction, "type") = c("single-objective")
attr(makeModifiedRastriginFunction, "tags") = c("single-objective", "multimodal", "continuous", "separable", "scalable")
