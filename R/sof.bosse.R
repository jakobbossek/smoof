#' @title
#' Bosse's Modified Griewank Function
#'
#' @description
#' Piecewise-smooth variant of the classical Griewank function.
#' The corresponding formula is:
#' \deqn{f(\mathbf{x}) =
#' 1 + \frac{1}{4000} \sum_{i=1}^{n} x_i^2
#' - \prod_{i=1}^{n} P_i(x_i)}
#' with
#' \deqn{P_i(x_i) =
#' |\cos(x_i/(2\sqrt{i}))| -
#' |\sin(x_i/(2\sqrt{i}))|}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100], i = 1, \ldots, n}.
#'
#' @return
#' An object of class \code{SingleObjectiveFunction}.
#'
#' @references
#' T. F. Bosse and H. M. Bücker,
#' A piecewise smooth version of the Griewank function,
#' Optimization Methods and Software, 2024.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeBosseModifiedGriewankFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)

  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Bosse Modified Griewank Function", sep = ""),
    id = paste0("bosse_modified_griewank_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)

      a = sum(x^2) / 4000

      i = seq_along(x)
      Pi = abs(cos(x / (2 * sqrt(i)))) - abs(sin(x / (2 * sqrt(i))))
      b = prod(Pi)

      return(1 + a - b)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    tags = attr(makeBosseModifiedGriewankFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeBosseModifiedGriewankFunction) = c("function", "smoof_generator")
attr(makeBosseModifiedGriewankFunction, "name") = c("BosseModifiedGriewank")
attr(makeBosseModifiedGriewankFunction, "type") = c("single-objective")
attr(makeBosseModifiedGriewankFunction, "tags") =
  c("single-objective", "continuous", "non-differentiable",
    "non-separable", "scalable", "multimodal")
