#' Brown Function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeBrownFunction = function(dimensions) {
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
  makeSingleObjectiveFunction(
    name = "Brown Function",
    fn = function(x) {
      i = 1:(length(x) - 1)
      a = x[i]^2
      b = x[i + 1]^2
      sum(a^(b + 1) + b^(a + 1))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(4, dimensions),
      vector = FALSE
      ),
    global.opt.params = global.opt.params,
    global.opt.value = 0
  )
}
