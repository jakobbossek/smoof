#' Bird Function
#'
#' Multimodal single-objective test function. The implementation is based on the
#' mathematical formulation
#' \deqn{f(\mathbf{x}) = (\mathbf{x}_1 - \mathbf{x}_2)^2 + \exp((1 - \sin(\mathbf{x}_1))^2)\cos(\mathbf{x}_2) + \exp((1 - \cos(\mathbf{x}_2))^2)\sin(\mathbf{x}_1).}
#' The function is restricted to two dimensions with \eqn{\mathbf{x}_i \in [-2\pi, 2\pi], i = 1, 2.}
#'
#' @template ret_smoof_single
#' @export
makeBirdFunction = function() {
  makeSingleObjectiveFunction(
    name = "Bird Function",
    fn = function(x) {
      a = (x[1] - x[2])^2
      b = exp((1 - sin(x[1])^2)) * cos(x[2])
      c = exp((1 - cos(x[2])^2)) * sin(x[1])
      return(a * b * c)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-2 * pi, -2 * pi),
      upper = c(2 * pi, 2 * pi),
      vector = FALSE
    ),
    tags = c("multimodal"),
    global.opt.params = matrix(
      c(4.701055751981055, 3.152946019601391,
        -1.582142172055011, -3.130246799635430),
      ncol = 2, byrow = TRUE),
    global.opt.value = -106.7645367198034
  )
}
