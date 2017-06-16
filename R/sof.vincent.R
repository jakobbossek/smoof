#' Inverted Vincent Function
#'
#' Single-objective test function based on the formula
#' \deqn{f(\mathbf{x}) = \frac{1}{n} \sum_{i = 1}^{n} \sin(10 \log(\mathbf{x}_i))}
#' subject to \eqn{\mathbf{x}_i \in [0.25, 10]} for \eqn{i = 1, \ldots, n}.
#'
#' @references Xiadong Li, Andries Engelbrecht, and Michael G. Epitropakis. Benchmark
#' functions for CEC2013 special session and competition on niching methods for
#' multimodal function optimization. Technical report, RMIT University, Evolutionary
#' Computation and Machine Learning Group, Australia, 2013.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeInvertedVincentFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = "Inverted Vincent Function",
    id = sprintf("invertedVincent_%id", dimensions),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(sin(10 * log(x))) / length(x)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0.25, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    minimize = FALSE,
    tags = attr(makeInvertedVincentFunction, "tags")
  )
}

class(makeInvertedVincentFunction) = c("function", "smoof_generator")
attr(makeInvertedVincentFunction, "name") = c("Inverted Vincent Mixture")
attr(makeInvertedVincentFunction, "type") = c("single-objective")
attr(makeInvertedVincentFunction, "tags") = c("single-objective", "continuous", "differentiable", "scalable", "multimodal")
