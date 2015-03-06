#' Egg Crate Function
#'
#' This single-objective function follows the definition
#' \deqn{f(\mathbf{x}) = \mathbf{x}_1^2 + \mathbf{x}_2^2 + 25(\sin^2(\mathbf{x}_1) + \sin^2(\mathbf{x}_2))}
#' with \eqn{\mathbf{x}_i \in [-5, 5]} for \eqn{i = 1, 2}.
#'
#'
#' @template ret_smoof_single
#' @export
makeEggCrateFunction = function() {
  makeSingleObjectiveFunction(
    name = "Egg Crate function",
    fn = function(x) {
      x[1]^2 + x[2]^2 + 25 * (sin(x[1])^2 + sin(x[2])^2)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-5, -5),
      upper = c(5, 5),
      vector = FALSE
    ),
    tags = c("continuous", "separable", "non-scalable"),
    global.opt.params = c(x1 = 0, x2 = 0),
    global.opt.value = 0
  )
}
