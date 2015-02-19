#' Price Function
#'
#' @export
makePriceFunction = function() {
    makeSingleObjectiveFunction(
        name = "Price Function",
        fn = function(x) {
            (2 * x[1]^3 * x[2] - x[2]^3)^2 + (6 * x[1] - x[2]^2 + x[2])^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        #FIXME: opt params at (0,0), (2, 4) and (1.464, -2.506)
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = 0
    )
}
