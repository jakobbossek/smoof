#' Double-Sum Function
#'
#' Also known as the rotated hyper-ellipsoid function. The formula is given by
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^n \left( \sum_{j=1}^{i} \mathbf{x}_j \right)^2}
#' with \eqn{\mathbf{x}_i \in [-65.536, 65.536], i = 1, \ldots, n}.
#'
#' @references H.-P. Schwefel. Evolution and Optimum Seeking.
#' John Wiley & Sons, New York, 1995.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeDoubleSumFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Double-Sum Function", sep = ""),
    fn = function(x) {
      # this is faster than the soobench C implementation
      sum(cumsum(x)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-65.536, dimensions),
      upper = rep(65.536, dimensions),
      vector = FALSE
    ),
    tags = attr(makeDoubleSumFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

class(makeDoubleSumFunction) = c("function", "smoof_generator")
attr(makeDoubleSumFunction, "name") = c("Double-Sum Function")
attr(makeDoubleSumFunction, "type") = c("single-objective")
attr(makeDoubleSumFunction, "tags") = c("convex", "unimodal", "differentiable", "separable", "scalable", "continuous")
