#' Matyas Function
#'
#' Two-dimensiona, unimodal test function
#' \deqn{f(\mathbf{x}) = 0.26 (\mathbf{x}_1^2 + \mathbf{x}_2^2) - 0.48\mathbf{x}_1\mathbf{x}_2}
#' subject to \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2}.
#'
#' @references A.-R. Hedar, Global Optimization Test Problems.
#'
#' @template ret_smoof_single
#' @export
makeMatyasFunction = function() {
  makeSingleObjectiveFunction(
    name = "Matyas Function",
    fn = function(x) {
      0.26 * (x[1]^2 + x[2]^2) - 0.48 * x[1] * x[2]
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "non-scalable", "unimodal"),
    global.opt.params = c(0, 0),
    global.opt.value = 0
  )
}
