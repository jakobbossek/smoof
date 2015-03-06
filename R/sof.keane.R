#' Keane Function
#'
#' Two-dimensional test function based on the defintion
#' \deqn{f(\mathbf{x}) = \frac{\sin^2(\mathbf{x}_1 - \mathbf{x}_2)\sin^2(\mathbf{x}_1 + \mathbf{x}_2)}{\sqrt{\mathbf{x}_1^2 + \mathbf{x}_2^2}}.}
#' The domain of definition is bounded by the box constraints
#' \eqn{\mathbf{x}_i \in [0, 10], i = 1, 2}.
#'
#' @template ret_smoof_single
#' @export
makeKeaneFunction = function() {
  makeSingleObjectiveFunction(
    name = "Keane Function",
    fn = function(x) {
      a = sin(x[1] - x[2])^2 * sin(x[1] + x[2])^2
      b = sqrt(x[1]^2 + x[2]^2)
      return (a / b)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(0, 0),
      upper = c(10, 10),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal"),
    global.opt.params = matrix(
      c(0, 1.39325,
        1.39325, 0),
      ncol = 2L, byrow = TRUE),
    global.opt.value = -0.673668
  )
}
