#' Bird Function
#'
#' @export
makeBirdFunction = function() {
    makeSingleObjectiveFunction(
        name = "Bird Function",
        fn = function(x) {
            a = (x[1] - x[2])^2
            b = exp((1 - sin(x[1])^2)) * cos(x[2])
            c = exp((1 - cos(x[2])^2)) * sin(x[1])
            return(a * b * c)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-2 * pi, -2 * pi),
            upper = c(2 * pi, 2 * pi),
            vector = FALSE
        )
        #FIXME: for global opt see http://infinity77.net/global_optimization/test_functions_nd_B.html#go_benchmark.BartelsConn
    )
}
