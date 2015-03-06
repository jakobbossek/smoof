#' Sphere Function
#'
#' Also known as the the \dQuote{De Jong function 1}. Convex, continous function
#' calculated via the formula
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} \mathbf{x}_i^2}
#' with box-constraings \eqn{\mathbf{x}_i \in [-5.12, 5.12], i = 1, \ldots, n}.
#'
#' @references M. A. Schumer, K. Steiglitz, Adaptive Step Size Random Search,
#' IEEE Transactions on Automatic Control. vol. 13, no. 3, pp. 270-276, 1968.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSphereFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sphere Function", sep = ""),
    fn = function(x) {
      sum(x^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.12, dimensions),
      upper = rep(5.12, dimensions),
      vector = FALSE
    ),
    tags = c("unimodal", "separable", "convex", "continuous", "differentiable", "scalable"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
