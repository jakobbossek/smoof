#' Cube Function
#'
#' The Cube Function is defined as follows:
#' \deqn{f(\mathbf{x}) = 100 (\mathbf{x}_2 - \mathbf{x}_1^3)^2 + (1 - \mathbf{x}_1)^2.}
#' The box-constraints are given by \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2.}
#'
#' @references A. Lavi, T. P. Vogel (eds), Recent Advances in Optimization
#' Techniques, John Wliley & Sons, 1966.
#'
#' @template ret_smoof_single
#' @export
makeCubeFunction = function() {
  makeSingleObjectiveFunction(
    name = "Cube Function",
    fn = function(x) {
      100 * (x[2] - x[1]^3)^2 + (1 - x[1])^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "non-scalable", "unimodal"),
    global.opt.params = c(x1 = -1, x2 = 1),
    global.opt.value = 0
  )
}
