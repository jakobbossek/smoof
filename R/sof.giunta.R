#' Giunta Function
#'
#' Multimodal test function based on the definition
#' \deqn{f(\mathbf{x}) = 0.6 + \sum_{i = 1}^{n} \left[\sin(\frac{16}{15} \mathbf{x}_i - 1) + \sin^2(\frac{16}{15}\mathbf{x}_i - 1) + \frac{1}{50} \sin(4(\frac{16}{15}\mathbf{x}_i - 1))\right]}
#' with box-constraints \eqn{\mathbf{x}_i \in [-1, 1]} for \eqn{i = 1, \ldots, n}.
#'
#' @references S. K. Mishra, Global Optimization By Differential Evolution and
#' Particle Swarm Methods: Evaluation On Some Benchmark Functions, Munich
#' Research Papers in Economics.
#'
#' @template ret_smoof_single
#' @export
#FIXME: this function is scalable, but global opt only known for 2D?
makeGiuntaFunction = function() {
  makeSingleObjectiveFunction(
    name = "2d Giunta function",
    fn = function(x) {
      a = 1.067 * x - 1
      b = sin(a)
      0.6 + sum(b + b^2 + 0.02 * sin(4 * a))
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = rep(-1, 2),
      upper = rep(1, 2),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "separable", "multimodal"),
    global.opt.params = rep(0.45834282, 2L),
    global.opt.value = 0.06463
  )
}
