#' DeckkersAarts function
#'
#' @template ret_smoof_single
#' @export
makeDeckkersAartsFunction = function() {
  makeSingleObjectiveFunction(
    name = "DeckkersAarts function",
    fn = function(x) {
      a = x[1]^2
      b = x[2]^2
      10^5 * a + b - (a + b)^2 + 1e-05 * (a + b)^4
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(-20, -20),
      upper = c(20, 20),
      vector = FALSE
    )
    #FIXME: for global opt see http://infinity77.net/global_optimization/test_functions_nd_D.html#go_benchmark.Deb01
  )
}
