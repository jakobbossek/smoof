#' Matyas Function
#'
#' @template ret_smoof_single
#' @export
makeMatyasFunction = function() {
    makeSingleObjectiveFunction(
        name = "Matyas Function",
        fn = function(x) {
            0.26 * (x[1]^2 + x[2]^2) - 0.48 * x[1] * x[2]
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = 0
    )
}
