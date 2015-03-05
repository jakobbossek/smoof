#' Chichinadze Function
#'
#' Continuous single-objective test function \eqn{f} with
#' \deqn{f(\mathbf{x}) = \mathbf{x}_1^2 - 12 \mathbf{x}_1 + 11 + 10\cos(0.5\pi\mathbf{x}_1) + 8\sin(2.5\pi\mathbf{x}_1) - (0.25)^{0.5}\exp(-0.5(\mathbf{x}_2 - 0.5)^2)}
#' with \eqn{-30 \leq \mathbf{x}_i \leq 30}.
#'
#' @template ret_smoof_single
#' @export
makeChichinadzeFunction = function() {
  makeSingleObjectiveFunction(
    name = "Chichinadze Function",
    fn = function(x) {
      x[1]^2 - 12 * x[1] + 11 + 10 * cos(pi * 0.5 * x[1]) + 8 * sin(5 * pi * x[1]) - exp(-0.5 * (x[2] - 0.5)^2) / sqrt(5)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-30, -10),
      upper = c(30, 10),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "separable", "non-scalable", "multimodal"),
    global.opt.params = c(x1 = 5.90133, x2 = 0.5),
    global.opt.value = -43.3159
  )
}
