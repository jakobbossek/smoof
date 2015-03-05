#' Adjiman function
#'
#' This two-dimensional multimodal test function follows the formula
#' \deqn{f(\mathbf{x}) = \cos(\mathbf{x}_1)\sin(\mathbf{x}_2) - \frac{\mathbf{x}_1}{(\mathbf{x}_2^2 + 1)}}
#' with \eqn{\mathbf{x}_1 \in [-1, 2], \mathbf{x}_2 \in [2, 1]}.
#'
#' @template ret_smoof_single
#' @export
makeAdjimanFunction = function() {
  makeSingleObjectiveFunction(
    name = "Adjiman function",
    fn = function(x) {
      cos(x[1]) * sin(x[2]) - x[1] / (x[2]^2 + 1)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-1, -1),
      upper = c(2, 1),
      vector = FALSE
      ),
    tags = c("multimodal"),
    global.opt.params = c(x1 = 2, x2 = 0.10578),
    global.opt.value = -2.02181
  )
}
