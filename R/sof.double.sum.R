#' Double-Sum function.
#'
#' Also known as the rotated hyper-ellipsoid function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeDoubleSumFunction = function(dimensions) {
  #FIXME: convex, unimodal
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Double-Sum function", sep = ""),
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
    global.opt.params = global.opt.params,
    global.opt.value = 0
  )
}
