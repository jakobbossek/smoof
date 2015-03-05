#' Hansen function
#'
#' @template ret_smoof_single
#' @export
makeHansenFunction = function() {
  makeSingleObjectiveFunction(
    name = "Hansen function",
    fn = function(x) {
      #FIXME: the paper (http://www.geocities.ws/eadorio/mvf.pdf)
      # states f(n,x) = ..., but n is never used. Maybe the limits of the sums are n+2 in general?
      i = j = 0:4
      a = sum((i + 1) * cos(i * x[1] + i + 1))
      b = sum((i + 1) * cos((i + 2) * x[2] + i + 1))
      return (a * b)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    )
    #FIXME: global minimun is -176.54, but what are the params?
  )
}
