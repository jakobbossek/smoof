#' Brent Function
#'
#' @template ret_smoof_single
#' @export
makeBrentFunction = function() {
  makeSingleObjectiveFunction(
    name = "Brent Function",
    fn = function(x) {
      (x[1] + 10)^2 + (x[2] + 10)^2 + exp(-x[1]^2 - x[2]^2)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
      ),
    global.opt.params = list("x1" = -10, "x2" = -10),
    global.opt.value = 0
  )
}
