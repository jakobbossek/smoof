#' @title
#' Himmelblau Function
#'
#' @description
#' Two-dimensional test function based on the function definition
#' \deqn{f(\mathbf{x}) = (\mathbf{x}_1^2 + \mathbf{x}_2 - 11)^2 + (\mathbf{x}_1 + \mathbf{x}_2^2 - 7)^2}
#' with box-constraints \eqn{\mathbf{x}_i \in [-5, 5], i = 1, 2}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Himmelblau Function.
#'
#' @references D. M. Himmelblau, Applied Nonlinear Programming, McGraw-Hill, 1972.
#'
#' @template ret_smoof_single
#' @export
makeHimmelblauFunction = function() {
  makeSingleObjectiveFunction(
    name = "Himmelblau Function",
    id = "himmelblau_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-5, -5),
      upper = c(5, 5),
      vector = TRUE
    ),
    tags = attr(makeHimmelblauFunction, "tags"),
    global.opt.params = c(x1 = 3, x2 = 2),
    global.opt.value = 0
  )
}

class(makeHimmelblauFunction) = c("function", "smoof_generator")
attr(makeHimmelblauFunction, "name") = c("Himmelblau")
attr(makeHimmelblauFunction, "type") = c("single-objective")
attr(makeHimmelblauFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
