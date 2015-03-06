#' Step function N. 2
#'
#' This function is based on the defintion
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} (\lfloor\mathbf{x}_i + 0.5\rfloor)^2}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100]} for \eqn{i = 1, \ldots, n}.
#'
#' @references T. Baeck, H. P. Schwefel, An Overview of Evolutionary Algorithm
#' for Parameter Optimization, Evolutionary Computation, vol. 1, no. 1, pp. 1-23, 1993.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeStepN2Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 2", sep = ""),
    fn = function(x) {
      sum((floor(x) + 0.5)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = FALSE
    ),
    tags = attr(makeStepN2Function, "tags"),
    global.opt.params = rep(0.5, dimensions),
    global.opt.value = 0
  )
}

class(makeStepN2Function) = c("function", "smoof_generator")
attr(makeStepN2Function, "name") = c("Step Function N. 2")
attr(makeStepN2Function, "type") = c("single-objective")
attr(makeStepN2Function, "tags") = c("discontinuous", "non-differentiable", "separable", "scalable", "unimodal")
