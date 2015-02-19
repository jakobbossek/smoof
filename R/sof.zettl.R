#' Zettl Function
#'
#' @template ret_smoof_single
#' @export
makeZettlFunction = function() {
    makeSingleObjectiveFunction(
        name = "Zettl Function",
        fn = function(x) {
            (x[1]^2 + x[2]^2 - 2 * x[1])^2 + 0.25 * x[1]
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0.0299, "x2" = 0),
        global.opt.value = -0.003791
    )
}
