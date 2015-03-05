#' Bochachevsky Function
#'
#' Highly multimodal single-objective test function. The mathematical formula is
#' given by
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n - 1} (\mathbf{x}_i^2 + 2 \mathbf{x}_{i + 1}^2 - 0.3\cos(3\pi\mathbf{x}_i) - 0.4\cos(4\pi\mathbf{x}_{i + 1}) + 0.7)}
#' with box-constraints \eqn{\mathbf{x}_i  \in [-100, 100]} for \eqn{i = 1, \ldots, n}.
#' The multimodality will be visible by \dQuote{zooming in} in the plot.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeBochachevskyFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = "Bochachevsky Function",
    fn = function(x) {
      i = 1:(length(x) - 1)
      sum(x[i] + 2 * x[i + 1]^2 - 0.3 * cos(3 * pi * x[i]) - 0.4 * cos(4 * pi * x[i + 1]) + 0.7)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-15, dimensions),
      upper = rep(15, dimensions),
      vector = FALSE
    ),
    tags = c("multimodal"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
