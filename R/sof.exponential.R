#' Exponential Function
#'
#' This scalable test function is based on the definition
#' \deqn{f(\mathbf{x}) = -\exp\left(-0.5 \sum_{i = 1}^{n} \mathbf{x}_i^2\right)}
#' with the box-constraints \eqn{\mathbf{x}_i \in [-1, 1], i = 1, \ldots, n}.
#'
#' @references S. Rahnamyan, H. R. Tizhoosh, N. M. M. Salama, Opposition-Based
#' Differential Evolution (ODE) with Variable Jumping Rate, IEEE Sympousim
#' Foundations Com- putation Intelligence, Honolulu, HI, pp. 81-88, 2007.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeExponentialFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Exponential Function", sep = ""),
    fn = function(x) {
      -exp(-0.5 * sum(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "scalable", "multimodal"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -1
  )
}
