#' Csendes Function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeCsendesFunction = function(dimensions) {
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
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
      vector = FALSE
    ),
    global.opt.params = global.opt.params,
    global.opt.value = 0
  )
}
