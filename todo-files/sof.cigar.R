#' Cigar function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeCigarFunction = function(dimensions) {
  #FIXME: type is convex, unimodal
  assertCount(dimensions)
  global.opt.params = as.list(rep(0, dimensions))
  names(global.opt.params) = paste("x", seq(dimensions), sep = "")
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Cigar function", sep = ""),
    fn = function(x) {
      w = c(1, rep(1e06, length(x) - 1L))
      sum(w * x^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = global.opt.params,
    global.opt.value = 0
  )
}
