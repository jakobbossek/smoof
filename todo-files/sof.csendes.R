#' Csendes Function
#'
#' Scalable, multimodal single-objective optimization function based on the
#' formula
#' \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} \mathbf{x}_i^6 \left(2 + \sin\left(\frac{1}{\mathbf{x}_i}\right)\right)}
#' subject to \eqn{-1 \leq \mathbf{x}_i \leq 1}.
#'
#' @references T. Csendes, D. Ratz, Subdivision Direction Selection in Interval
#' Methods for Global Optimization, SIAM Journal on Numerical Analysis, vol. 34,
#' no. 3, pp. 922-938.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
#FIXME: global opt is in (0, ..., 0), but function is not defined for this params!
makeCsendesFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = "Csendes Function",
    fn = function(x) {
      sum(x^6 * (2 + sin(1 / x)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = c(-1, -1),
      upper = c(1, 1),
      vector = TRUE
    ),
    tags = attr(makeCsendesFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeCsendesFunction) = c("function", "smoof_generator")
attr(makeCsendesFunction, "name") = c("Csendes Function")
attr(makeCsendesFunction, "type") = c("single-objective")
attr(makeCsendesFunction, "tags") = c("continuous", "differentiable", "separable", "scalable", "multimodal")
