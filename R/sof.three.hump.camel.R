#' Three-Hump Camel Function
#'
#' @template ret_smoof_single
#' @export
makeThreeHumpCamelFunction = function() {
    makeSingleObjectiveFunction(
        name = "Three-Hump Camel Function",
        fn = function(x) {
            2 * x[1]^2 - 1.05 * x[1]^4 + (x[1]^6) / 6 + x[1] * x[2] + x[2]^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5, -5),
            upper = c(5, 5),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = 0
    )
}
