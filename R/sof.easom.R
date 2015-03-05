#' Easom Function
#'
#' Unimodal function with its global optimum in the center of the search space.
#' The attraction area of the global optimum is very small in relation to the
#' search space:
#' \deqn{f(\mathbf{x}) = -\cos(\mathbf{x}_1)\cos(\mathbf{x}_2)\exp\left(-\left((\mathbf{x}_1 - \pi)^2 + (\mathbf{x}_2 - pi)^2\right)\right)}
#' with \eqn{\mathbf{x}_i \in [-100, 100], i = 1,2.}
#'
#' @references Easom, E. E.: A survey of global optimization techniques. M. Eng.
#' thesis, University of Louisville, Louisville, KY, 1990.
#'
#' @template ret_smoof_single
#' @export
makeEasomFunction = function() {
  makeSingleObjectiveFunction(
    name = "Easom Function",
    fn = function(x) {
      -cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-100, -100),
      upper = c(100, 100),
      vector = FALSE
    ),
    tags = c("continuous", "differentiable", "separable", "non-scalable", "multimodal"),
    global.opt.params = c(pi, pi),
    global.opt.value = -1
  )
}
