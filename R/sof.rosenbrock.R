#' @title
#' Rosenbrock Function
#'
#' @description
#' Also known as the \dQuote{De Jong's function 2} or the \dQuote{(Rosenbrock)
#' banana/valley function} due to its shape. The global optimum is located within
#' a large flat valley and thus it is hard for optimization algorithms to find it.
#' The following formula underlies the implementation:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n-1} 100 \cdot (\mathbf{x}_{i+1} - \mathbf{x}_i^2)^2 + (1 - \mathbf{x}_i)^2.}
#' The domain is given by the constraints \eqn{\mathbf{x}_i \in [-30, 30], i = 1, \ldots, n}.
#'
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Rosenbrock Function.
#'
#' @references H. H. Rosenbrock, An Automatic Method for Finding the Greatest or
#' least Value of a Function, Computer Journal, vol. 3, no. 3, pp. 175-184, 1960.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeRosenbrockFunction = function(dimensions) {
  checkmate::assertInt(dimensions, lower = 2)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rosenbrock Function", sep = ""),
    id = paste0("rosenbrock_", dimensions, "d"),
    fn = function(x) {
      checkNumericInput(x, dimensions)
      i = seq_len(length(x) - 1L)
      sum(100 * (x[i]^2 - x[i + 1])^2 + (x[i] - 1)^2)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    tags = attr(makeRosenbrockFunction, "tags"),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

class(makeRosenbrockFunction) = c("function", "smoof_generator")
attr(makeRosenbrockFunction, "name") = c("Rosenbrock")
attr(makeRosenbrockFunction, "type") = c("single-objective")
attr(makeRosenbrockFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "scalable", "multimodal")
