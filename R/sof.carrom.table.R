#' Carrom Table Function
#'
#' This function is defined as follows:
#' \deqn{f(\mathbf{x}) = -\frac{1}{30} \left((\cos(\mathbf{x}_1)\exp(|1 - ((\mathbf{x}_1^2 + \mathbf{x}_2^2)^{0.5} / \pi)^2)|\right).}
#' The box-constraints are given by \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2}.
#'
#' @references S. K. Mishra, Global Optimization By Differential Evolution and
#' Particle Swarm Methods: Evaluation On Some Benchmark Functions, Munich
#' Research Papers in Economics.
#'
#' @template ret_smoof_single
#' @export
makeCarromTableFunction = function() {
  makeSingleObjectiveFunction(
    name = "Carrom Table Function",
    fn = function(x) {
      (-1 / 30) * exp(2 * abs(1 - (sqrt(x[1]^2 + x[2]^2) / pi))) * cos(x[1])^2 * cos(x[2])^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal"),
    global.opt.params = matrix(
      c(9.646157266348881, 9.646134286497169,
        -9.646157266348881, 9.646134286497169,
        9.646157266348881, -9.646134286497169,
        -9.646157266348881, -9.646134286497169),
      ncol = 2L, byrow = TRUE),
    global.opt.value = -24.1568155
  )
}
