#' @title
#' Schaffer Function N. 4
#' 
#' @description
#' Second function by Schaffer. The definition is given by the formula
#' \deqn{f(\mathbf{x}) = 0.5 + \frac{\cos^2(sin(|\mathbf{x}_1^2 - \mathbf{x}_2^2|)) - 0.5}{(1 + 0.001(\mathbf{x}_1^2 + \mathbf{x}_2^2))^2}}
#' subject to \eqn{\mathbf{x}_i \in [-100, 100], i = 1, 2}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Schaffer N. 4 Function.
#'
#' @references S. K. Mishra, Some New Test Functions For Global Optimization
#' And Performance of Repulsive Particle Swarm Method.
#'
#' @template ret_smoof_single
#' @export
makeSchafferN4Function = function() {
  makeSingleObjectiveFunction(
    name = "Schaffer Function N. 4",
    id = "schaffer04_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      a = x[1]^2
      b = x[2]^2
      0.5 + (cos(sin(abs(a - b)))^2 - 0.5) / (1 + 0.001 * (a + b))^2
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-100, -100),
      upper = c(100, 100),
      vector = TRUE
    ),
    tags = attr(makeSchafferN4Function, "tags"),
    global.opt.params = c(0, 1.253115),
    global.opt.value = 0.292579
  )
}

class(makeSchafferN4Function) = c("function", "smoof_generator")
attr(makeSchafferN4Function, "name") = c("Schaffer N. 4")
attr(makeSchafferN4Function, "type") = c("single-objective")
attr(makeSchafferN4Function, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "unimodal")
