#' Bukin function N. 6
#'
#' @export
makeBukinN6Function = function() {
    makeSingleObjectiveFunction(
        name = "Bukin function N.6",
        fn = function(x) {
            100 * sqrt(abs(x[2] - 0.01 * x[1]^2)) + 0.01 * abs(x[1] + 10)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-15, -3),
            upper = c(-5, 3),
            vector = FALSE
        ),
        global.opt.params = list("x1" = -10, "x2" = 1),
        global.opt.value = 0
    )
}