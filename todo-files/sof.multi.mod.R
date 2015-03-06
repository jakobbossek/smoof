#' MultiMod function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeMultiModFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d MultiMod function", sep = ""),
    fn = function(x) {
      a = abs(x)
      sum(a) * prod(a)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = FALSE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}
