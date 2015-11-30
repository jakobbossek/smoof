#' Step function N. 1
#'
#' This function is based on the defintion
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} (\lfloor |\mathbf{x}_i|\rfloor)}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100]} for \eqn{i = 1, \ldots, n}.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeStepN1Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 1", sep = ""),
    fn = function(x) {
      sum(floor(abs(x)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    tags = attr(makeStepN1Function, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeStepN1Function) = c("function", "smoof_generator")
attr(makeStepN1Function, "name") = c("Step Function N. 1")
attr(makeStepN1Function, "type") = c("single-objective")
attr(makeStepN1Function, "tags") = c("single-objective", "discontinuous", "non-differentiable", "separable", "scalable", "unimodal")
