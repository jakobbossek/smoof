#' Griewank Function
#'
#' Highly multimodal function with a lot of regularly distributed local minima.
#' The corresponding formula is:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} \frac{\mathbf{x}_i^2}{4000} - \prod_{i=1}^{n} \cos\left(\frac{\mathbf{x}_i}{\sqrt{i}}\right) + 1}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100], i = 1, \ldots, n}.
#'
#' @references A. O. Griewank, Generalized Descent for Global Optimization,
#' Journal of Optimization Theory and Applications, vol. 34, no. 1,
#' pp. 11-39, 1981.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeGriewankFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Griewank Function", sep = ""),
    fn = function(x) {
      a = sum(x^2) / 4000
      b = prod(cos(x / sqrt(1:length(x))))
      return(a - b + 1)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "scalable", "multimodal"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
