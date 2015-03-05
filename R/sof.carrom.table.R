#' Carrom table function.
#'
#' @template ret_smoof_single
#' @export
makeCarromTableFunction = function() {
  makeSingleObjectiveFunction(
    name = "Carrom table function",
    fn = function(x) {
      (-1 / 30) * exp(2 * abs(1 - (sqrt(x[1]^2 + x[2]^2) / pi))) * cos(x[1])^2 * cos(x[2])^2
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-10, -10),
      upper = c(10, 10),
      vector = FALSE
    )
    #FIXME: for global opt see http://infinity77.net/global_optimization/test_functions_nd_C.html#go_benchmark.CarromTable
  )
}
