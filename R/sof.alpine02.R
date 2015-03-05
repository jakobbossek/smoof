#' Alpine02 function
#'
#' Another multimodal optimization test function. The implementation is based on
#' the formula
#' \deqn{f(\mathbf{x}) = \prod_{i = 1}^{n} \sqrt{\mathbf{x}_i}\sin(\mathbf{x}_i)}
#' with \eqn{\mathbf{x}_i \in [0, 10]} for \eqn{i = 1, \ldots, n}.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
#FIXME: sometimes the upper bound is a variable xmax: the higher xmax, the more
# local optima there are. But there is always exactly one global optimum.
makeAlpine02Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Alpine02 function", sep = ""),
    fn = function(x) {
      prod(sqrt(x) * sin(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(10, dimensions),
      vector = FALSE
      ),
    tags = c("multimodal"),
    global.opt.params = rep(7.917, dimensions),
    global.opt.value = -6.1295
  )
}
