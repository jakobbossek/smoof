#' @title
#' Griewank Function
#'
#' @description
#' Highly multi-modal function with a lot of regularly distributed local minima.
#' The corresponding formula is:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} \frac{\mathbf{x}_i^2}{4000} - \prod_{i=1}^{n} \cos\left(\frac{\mathbf{x}_i}{\sqrt{i}}\right) + 1}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100], i = 1, \ldots, n}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Griewank Function.
#'
#' @references A. O. Griewank, Generalized Descent for Global Optimization,
#' Journal of Optimization Theory and Applications, vol. 34, no. 1,
#' pp. 11-39, 1981.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeGriewankFunction = function(dimensions) {
  checkmate::assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Griewank Function", sep = ""),
    id = paste0("griewank_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      a = sum(x^2) / 4000
      b = prod(cos(x / sqrt(1:length(x))))
      return(a - b + 1)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    tags = attr(makeGriewankFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeGriewankFunction) = c("function", "smoof_generator")
attr(makeGriewankFunction, "name") = c("Griewank")
attr(makeGriewankFunction, "type") = c("single-objective")
attr(makeGriewankFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "scalable", "multimodal")
