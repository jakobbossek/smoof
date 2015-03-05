#' Freudenstein Roth Function
#'
#' @template ret_smoof_single
#' @export
makeFreudensteinRothFunction = function() {
  makeSingleObjectiveFunction(
    name = "Freudenstein Roth Function",
    fn = function(x) {
      (x[1] - 13 + ((5 - x[2]) * x[2]) - 2)^2 + (x[1] - 29 + ((x[2] + 1) * x[2] - 14) * x[2])^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    ),
    global.opt.params = list("x1" = 5, "x2" = 4),
    global.opt.value = 0
  )
}
