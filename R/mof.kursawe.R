#' @title
#' Kursawe Function
#'
#' @description
#' Builds and returns the bi-objective Kursawe test problem.
#'
#' The Kursawe test problem is defined as follows:
#'
#' Minimize \eqn{f_1(\mathbf{x}) = \sum\limits_{i = 1}^{n - 1} (-10 \cdot \exp(-0.2 \cdot \sqrt{x_i^2 + x_{i + 1}^2})),}{
#' f[1](X) = (-10 * exp(-0.2 * sqrt(x[1]^2 + x[2]^2)) + ... + (-10 * exp(-0.2 * sqrt(x[n - 1]^2 + x[n]^2))}
#'
#' Minimize \eqn{f_2(\mathbf{x}) = \sum\limits_{i = 1}^{n} ( |x_i|^{0.8} + 5 \cdot \sin^3(x_i)),}{
#' f[2](X) = sum(abs(x)^0.8 + 5 * (sin(x))^3)}
#'
#' with \eqn{-5 \leq x_i \leq 5}{-5 <= x[i] <= 5}, for \eqn{i = 1, 2, \ldots, n,}{i = 1, 2, ..., n}.
#'
#' @references F. Kursawe. A Variant of Evolution Strategies for Vector
#' Optimization. Proceedings of the International Conference on Parallel
#' Problem Solving from Nature, pp. 193-197, Springer, 1990.
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeKursaweFunction = function(dimensions) {
  assertInt(dimensions, lower = 2L)

  # C++ implementation
  fn = function(x) {
    assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE, lower = -5, upper = 5)
    kursawe(x)
  }

  makeMultiObjectiveFunction(
    name = "Kursawe Function",
    id = paste0("kursawe_", dimensions, "d_2o"),
    description = "Kursawe",
    fn = fn,
    par.set =  makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
    n.objectives = 2,
    ref.point = c(-2.4, 17.25)
  )
}

class(makeKursaweFunction) = c("function", "smoof_generator")
attr(makeKursaweFunction, "name") = c("Kursawe")
attr(makeKursaweFunction, "type") = c("multi-objective")
attr(makeKursaweFunction, "tags") = c("multi-objective")
