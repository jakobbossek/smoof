#' Pen Holder Function
#'
#' Single-objective two-dimensional test function. The formula is given as
#' \deqn{f(\mathbf{x}) = -\exp(|\cos(\mathbf{x}_1)\cos(mathbf{x}_2)\exp(1 - \sqrt{\mathbf{x}_1^2 + \mathbf{x}_2^2}/\pi)|^{-1})}
#' subject to the constraints \eqn{\mathbf{x}_i \in [-11, 11], i = 1, 2.}
#'
#' @references S. K. Mishra, Global Optimization By Differential Evolution and
#' Particle Swarm Methods: Evaluation On Some Benchmark Functions, Munich Research
#' Papers in Economics.
#'
#' @template ret_smoof_single
#' @export
#FIXME: global optimum not approximated good enough by formula
makePenHolderFunction = function() {
  makeSingleObjectiveFunction(
    name = "Pen Holder Function",
    fn = function(x) {
      a = exp(abs(1 - (sqrt(sum(x^2)) / pi)))
      b = abs(prod(cos(x)) * a)
      -exp(1 / b)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-11, -11),
      upper = c(11, 11),
      vector = TRUE
    ),
    tags = attr(makePenHolderFunction, "tags"),
    global.opt.params = matrix(
      c(9.646168, 9.646168,
        -9.646168, 9.646168,
        9.646168, -9.646168,
        -9.646168, -9.646168),
      ncol = 2L, byrow = TRUE),
    global.opt.value = -0.96354
  )
}

class(makePenHolderFunction) = c("function", "smoof_generator")
attr(makePenHolderFunction, "name") = c("Pen Holder Function")
attr(makePenHolderFunction, "type") = c("single-objective")
attr(makePenHolderFunction, "tags") = c("continuous", "differentiable", "non-separable", "non-scalable", "multimodal")
