#' AMGM function
#'
#' Aritmetic Mean - Geometric Mean Equality problem.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeAMGMFunction = function(dimensions) {
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d AMGM function", sep = ""),
    fn = function(x) {
      (mean(x) - prod(x)^(1 / length(x))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(10, dimensions),
      vector = FALSE
      ),
    global.opt.params = global.opt.params,
    global.opt.value = -6.1295
  )
}
