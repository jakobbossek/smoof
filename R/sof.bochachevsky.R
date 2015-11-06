#' Bochachevsky Function 1
#'
#' Highly multimodal single-objective test function. The mathematical formula is
#' given by
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n - 1} (\mathbf{x}_i^2 + 2 \mathbf{x}_{i + 1}^2 - 0.3\cos(3\pi\mathbf{x}_i) - 0.4\cos(4\pi\mathbf{x}_{i + 1}) + 0.7)}
#' with box-constraints \eqn{\mathbf{x}_i  \in [-100, 100]} for \eqn{i = 1, \ldots, n}.
#' The multimodality will be visible by \dQuote{zooming in} in the plot.
#'
#' @references  I. O. Bohachevsky, M. E. Johnson, M. L. Stein, General Simulated
#' Annealing for Function Optimization, Technometrics, vol. 28, no. 3, pp. 209-217, 1986.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeBochachevskyFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = "Bochachevsky Function 1",
    fn = function(x) {
      i = 1:(length(x) - 1)
      sum(x[i]^2 + 2 * x[i + 1]^2 - 0.3 * cos(3 * pi * x[i]) - 0.4 * cos(4 * pi * x[i + 1]) + 0.7)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-15, dimensions),
      upper = rep(15, dimensions),
      vector = TRUE
    ),
    tags = attr(makeBochachevskyFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeBochachevskyFunction) = c("function", "smoof_generator")
attr(makeBochachevskyFunction, "name") = c("Bochachevsky Function")
attr(makeBochachevskyFunction, "type") = c("single-objective")
attr(makeBochachevskyFunction, "tags") = c("continuous", "differentiable", "separable", "scalable", "multimodal")
