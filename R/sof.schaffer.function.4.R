#' Schaffer Function N. 4
#'
#' Second function by Schaffer. The defintion is given by the formula
#' \deqn{f(\mathbf{x}) = 0.5 + \frac{\cos^2(sin(|\mathbf{x}_1^2 - \mathbf{x}_2^2|)) - 0.5}{(1 + 0.001(\mathbf{x}_1^2 + \mathbf{x}_2^2))^2}}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100], i = 1, 2}.
#'
#' @references S. K. Mishra, Some New Test Functions For Global Optimization
#' And Performance of Repulsive Particle Swarm Method.
#'
#' @template ret_smoof_single
#' @export
makeSchafferN4Function = function() {
  makeSingleObjectiveFunction(
    name = "Schaffer Function N. 4",
    fn = function(x) {
      a = x[1]^2
      b = x[2]^2
      0.5 + (cos(sin(abs(a - b)))^2 - 0.5) / (1 + 0.001 * (a + b))^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-100, -100),
      upper = c(100, 100),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "non-separable", "non-scalable", "unimodal"),
    global.opt.params = c(x1 = 0, x2 = 1.253115),
    global.opt.value = 0.292579
  )
}
