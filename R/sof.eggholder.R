#' Egg Holder function
#'
#' The Egg Holder function is a difficult to optimize function based on the
#' definition
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n-1} \left[-(\mathbf{x}_{i + 1} + 47)\sin\sqrt{|\mathbf{x}_{i + 1} + 0.5 \mathbf{x}_{i} + 47|} - \mathbf{x}_i\sin(\sqrt{|\mathbf{x}_i - (\mathbf{x}_{i + 1} - 47)|})\right]}
#' subject to \eqn{-512 \leq \mathbf{x}_i \leq 512} for \eqn{i = 1, \ldots, n}.
#'
#' @template ret_smoof_single
#' @export
#FIXME: we do not provide the dimensions attribute here, since we do not know
# the global optimum params for dimensions > 2
makeEggholderFunction = function() {
  makeSingleObjectiveFunction(
    name = "Egg Holder Function",
    fn = function(x) {
      i = 1L
      #i = 1:(length(x) - 1L)
      sum(-(x[i + 1] + 47) * sin(sqrt(abs(x[i + 1] + 0.5 * x[i] + 47))) - x[i] * sin(sqrt(abs(x[i] - (x[i + 1] + 47)))))
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-512, -512),
      upper = c(512, 512),
      vector = FALSE
    ),
    tags = attr(makeEggholderFunction, "tags"),
    global.opt.params = c(x1 = 512, x2 = 404.2319),
    global.opt.value = -959.64
  )
}

class(makeEggholderFunction) = c("function", "smoof_generator")
attr(makeEggholderFunction, "name") = c("Egg Holder Function")
attr(makeEggholderFunction, "type") = c("single-objective")
attr(makeEggholderFunction, "tags") = c("continuous", "differentiable", "non-separable", "scalable", "multimodal")
