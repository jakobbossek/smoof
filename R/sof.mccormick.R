#' McCormick Function
#'
#' @template ret_smoof_single
#' @export
makeMcCormickFunction = function() {
    makeSingleObjectiveFunction(
        name = "McCormick Function",
        fn = function(x) {
            sin(x[1] + x[2]) + (x[1] - x[2])^2 - 1.5 * x[1] + 2.5 * x[2] + 1
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-1.5, -3),
            upper = c(4, 4),
            vector = FALSE
        ),
        global.opt.params = list("x1" = -0.54719, "x2" = -1.54719),
        global.opt.value = -1.9133
    )
}
