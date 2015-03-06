#' AMGM function
#'
#' Aritmetic Mean - Geometric Mean Equality problem. Multimodal problem defined
#' by the formula
#' \deqn{f(\mathbf{x}) = \left(\frac{1}{n} \sum_{i = 1}^{n} \mathbf{x}_i - \sqrt[n]{\prod_{i = 1}^n \mathbf{x}_i}\right)^2}
#' within the constraints \eqn{\mathbf{x}_i \in [0, 10]} for \eqn{i = 1, \ldots, n}.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeAMGMFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d AMGM function", sep = ""),
    fn = function(x) {
      (mean(x) - prod(x)^(1 / length(x)))^2
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(10, dimensions),
      vector = FALSE
      ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -6.1295
  )
}
