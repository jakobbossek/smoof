#' Bukin function N. 4
#'
#' @template ret_smoof_single
#' @export
makeBukinN4Function = function() {
  makeSingleObjectiveFunction(
    name = "Bukin function N. 4",
    fn = function(x) {
      100 * x[2]^2 + 0.01 * abs(x[1] + 10)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-15, -3),
      upper = c(-5, 3),
      vector = FALSE
      ),
    global.opt.params = list("x1" = -10, "x2" = 0),
    global.opt.value = 0
  )
}
