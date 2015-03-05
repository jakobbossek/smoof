#' Giunta function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeGiuntaFunction = function(dimensions) {
  #FIXME: type is convex, unimodal
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Giunta function", sep = ""),
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
    global.opt.params = global.opt.params,
    global.opt.value = 0.06447042053690566
  )
}
