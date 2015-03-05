#' Shubert Function
#'
#' @template ret_smoof_single
#' @export
makeShubertFunction = function() {
  makeSingleObjectiveFunction(
    name = "Shubert Function",
    fn = function(x) {
      n = length(x)
      w = 1:n
      a = sum(w * cos((w + 1) * x[1] + w))
      b = sum(w * cos((w + 1) * x[2] + w))
      return (a * b)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    )
    #FIXME: global opt?
  )
}
