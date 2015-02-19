#' Decanomial function
#'
#' @export
makeDecanomialFunction = function() {
    makeSingleObjectiveFunction(
        name = "Decanomial function",
        fn = function(x) {
            0.001 * (abs(x[2]^4 + 12 * x[2]^3 + 54 * x[2]^2 + 108 * x[2] + 81) + abs(x[1]^10 - 20 * x[1]^9 + 180 * x[1]^8 - 960 * x[1]^7 + 3360 * x[1]^6 - 8064 * x[1]^5 + 13340 * x[1]^4 - 15360 * x[1]^3 + 11520 * x[1]^2 - 5120 * x[1] + 2624))^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 2, "x2" = -3),
        global.opt.value = 0
    )
}
