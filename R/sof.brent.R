#' @title
#' Brent Function
#'
#' @description
#' Single-objective two-dimensional test function. The formula is given as
#' \deqn{f(\mathbf{x}) = (\mathbf{x}_1 + 10)^2 + (\mathbf{x}_2 + 10)^2 + exp(-\mathbf{x}_1^2 - \mathbf{x}_2^2)}
#' subject to the constraints \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2.}
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Brent Function.
#'
#' @references F. H. Branin Jr., Widely Convergent Method of Finding Multiple
#' Solutions of Simul- taneous Nonlinear Equations, IBM Journal of Research and
#' Development, vol. 16, no. 5, pp. 504-522, 1972.
#'
#' @template ret_smoof_single
#' @export
makeBrentFunction = function() {
  makeSingleObjectiveFunction(
    name = "Brent Function",
    id = "brent_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      (x[1] + 10)^2 + (x[2] + 10)^2 + exp(-x[1]^2 - x[2]^2)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = TRUE
    ),
    tags = attr(makeBrentFunction, "tags"),
    global.opt.params = c(-10, -10),
    global.opt.value = 0
  )
}

class(makeBrentFunction) = c("function", "smoof_generator")
attr(makeBrentFunction, "name") = c("Brent")
attr(makeBrentFunction, "type") = c("single-objective")
attr(makeBrentFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "unimodal")
