#' @title
#' Matyas Function
#'
#' @description
#' Two-dimensional, uni-modal test function
#' \deqn{f(\mathbf{x}) = 0.26 (\mathbf{x}_1^2 + \mathbf{x}_2^2) - 0.48\mathbf{x}_1\mathbf{x}_2}
#' subject to \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Matyas Function.
#'
#' @references A.-R. Hedar, Global Optimization Test Problems.
#'
#' @template ret_smoof_single
#' @export
makeMatyasFunction = function() {
  makeSingleObjectiveFunction(
    name = "Matyas Function",
    id = "matyas_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      0.26 * (x[1]^2 + x[2]^2) - 0.48 * x[1] * x[2]
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = TRUE
    ),
    tags = attr(makeMatyasFunction, "tags"),
    global.opt.params = c(0, 0),
    global.opt.value = 0
  )
}

class(makeMatyasFunction) = c("function", "smoof_generator")
attr(makeMatyasFunction, "name") = c("Matyas")
attr(makeMatyasFunction, "type") = c("single-objective")
attr(makeMatyasFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "unimodal")
