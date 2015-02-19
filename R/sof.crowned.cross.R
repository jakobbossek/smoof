#' CrownedCross Function
#'
#' See also CrossInTray function.
#' @export
makeCrownedCrossFunction = function() {
    makeSingleObjectiveFunction(
        name = "CrownedCross Function",
        fn = function(x) {
            a = exp(abs(100 - (sqrt(x[1]^2 + x[2]^2)) / pi))
            0.0001 * (abs(a * sin(x[1]) * sin(x[2])) + 1)^(0.1)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-15, -15),
            upper = c(15, 15),
            vector = FALSE
        )
    )
    #FIXME: for global opt see http://infinity77.net/global_optimization/test_functions_nd_C.html#go_benchmark.CarromTable
}
