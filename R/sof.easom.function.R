#' Easom Function
#'
#' @export
makeEasomFunction = function() {
    makeSingleObjectiveFunction(
        name = "Easom Camel Function",
        fn = function(x) {
            -cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-100, -100),
            upper = c(100, 100),
            vector = FALSE
        ),
        global.opt.params = list("x1" = pi, "x2" = pi),
        global.opt.value = -1
    )
}
