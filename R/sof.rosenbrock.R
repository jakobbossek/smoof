#' Rosenbrock function
#'
#' Also known as the \dQuote{De Jong's function 2} or the \dQuote{(Rosenbrock)
#' banana/valley function} due to its shape. The global optimum is located within
#' a large flat valley and thus it is hard for optimization algorithms to find it.
#' The following formula underlies the implementation:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n-1} 100 \cdot (\mathbf{x}_{i+1} - \mathbf{x}_i^2)^2 + (1 - \mathbf{x}_i)^2.}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeRosenbrockFunction = function(dimensions) {
  assertCount(dimensions, na.ok = FALSE)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rosenbrock function", sep = ""),
    fn = function(x) {
      i = 1:(length(x) - 1L)
      sum(100 * (x[i + 1] - x[i]^2)^2 + (x[i] - 1)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-2.048, dimensions),
      upper = rep(2.048, dimensions),
      vector = FALSE
    ),
    tags = c("unimodal", "convex", "continuous"),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}
