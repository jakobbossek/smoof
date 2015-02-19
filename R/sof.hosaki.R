#' Hosaki function
#'
#' @template ret_smoof_single
#' @export
makeHosakiFunction = function() {
    makeSingleObjectiveFunction(
        name = "Hosaki function",
        fn = function(x) {
            (1 + 8 * x[1] + 7 * x[1]^2 - 7 * x[1]^3 / 3 + 0.25 * x[1]^4) * x[2]^2 * exp(-x[1])
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(0, 0),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 4, "x2" = 2),
        global.opt.value = -2.3458
    )
}
