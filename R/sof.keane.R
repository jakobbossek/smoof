#' @title
#' Keane Function
#'
#' @description
#' Two-dimensional test function based on the definition
#' \deqn{f(\mathbf{x}) = \frac{\sin^2(\mathbf{x}_1 - \mathbf{x}_2)\sin^2(\mathbf{x}_1 + \mathbf{x}_2)}{\sqrt{\mathbf{x}_1^2 + \mathbf{x}_2^2}}.}
#' The domain of definition is bounded by the box constraints
#' \eqn{\mathbf{x}_i \in [0, 10], i = 1, 2}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Keane Function.
#'
#' @template ret_smoof_single
#' @export
makeKeaneFunction = function() {
  makeSingleObjectiveFunction(
    name = "Keane Function",
    id = "keane_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      a = sin(x[1] - x[2])^2 * sin(x[1] + x[2])^2
      b = sqrt(x[1]^2 + x[2]^2)
      return (a / b)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(0, 0),
      upper = c(10, 10),
      vector = TRUE
    ),
    minimize = FALSE,
    tags = attr(makeKeaneFunction, "tags"),
    global.opt.params = matrix(
      c(0, 1.39325,
        1.39325, 0),
      ncol = 2L, byrow = TRUE),
    global.opt.value = 0.673668
  )
}

class(makeKeaneFunction) = c("function", "smoof_generator")
attr(makeKeaneFunction, "name") = c("Keane")
attr(makeKeaneFunction, "type") = c("single-objective")
attr(makeKeaneFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
