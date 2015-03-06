#' Step function N. 3
#'
#' This function is based on the defintion
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} (\lfloor \mathbf{x}_i^2\rfloor)}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100]} for \eqn{i = 1, \ldots, n}.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeStepN3Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 3", sep = ""),
    fn = function(x) {
      sum(floor(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = FALSE
    ),
    tags = c("discontinuous", "non-differentiable", "separable", "scalable", "unimodal"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
