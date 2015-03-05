#' Keane function
#'
#' @template ret_smoof_single
# @export
#FIMXE: http://infinity77.net/global_optimization/test_functions_nd_K.html#go_benchmark.Kowalik
# Somethings incorrect here. The function is defined with additional constrains in the original version
makeKeaneFunction = function() {
  makeSingleObjectiveFunction(
    name = "Keane function",
    fn = function(x) {
      a = sin(x[1] - x[2])^2 * sin(x[1] + x[2])^2
      b = sqrt(x[1]^2 + x[2]^2)
      return (a / b)
    },
    par.set = makeNumericParamSet(
      len = 2L,
      id = "x",
      lower = c(0, 0),
      upper = c(10, 10),
      vector = FALSE
    ),
    global.opt.params = list("x1" = 0, "x2" = 1.39325),
    global.opt.value = 0.673668
  )
}
