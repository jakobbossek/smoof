#' @title Jennrich-Sampson function.
#'
#' @description Two-dimensional test function based on the formula
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{10} \left[2 + 2i - (e^{ix_1} + e^{ix_2})\right]^2}
#' with \eqn{\mathbf{x}_1, \mathbf{x}_2 \in [-1, 1]}.
#' 
#' @return
#' An object of class \code{SingleObjectiveFunction}, representing the Jennrich-Sampson Function.
#'
#' @references See \url{https://al-roomi.org/benchmarks/unconstrained/2-dimensions/134-jennrich-sampson-s-function}.
#'
#' @template ret_smoof_single
#' @export
makeJennrichSampsonFunction = function() {
  makeSingleObjectiveFunction(
    name = "Jennrich-Sampson Function",
    id = "jennrichSampson_2d",
    fn = function(x) {
      checkNumericInput(x, 2L)
      i = 1:10
      sum((2 + 2 * i - (exp(i * x[1]) + exp(i * x[2])))^2)
    },
    par.set = ParamHelpers::makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-1, -1),
      upper = c(1, 1),
      vector = TRUE
    ),
    tags = attr(makeJennrichSampsonFunction, "tags"),
    global.opt.params = c(0.25782521321500883, 0.25782521381356827),
    global.opt.value = 124.36218235561473896
  )
}

class(makeJennrichSampsonFunction) = c("function", "smoof_generator")
attr(makeJennrichSampsonFunction, "name") = c("Jennrich-Sampson")
attr(makeJennrichSampsonFunction, "type") = c("single-objective")
attr(makeJennrichSampsonFunction, "tags") = c("single-objective", "continuous", "differentiable", "non-separable", "non-scalable", "unimodal")
