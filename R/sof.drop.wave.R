#' Drop-Wave function
#'
#' @template ret_smoof_single
#' @export
makeDropWaveFunction = function() {
    makeSingleObjectiveFunction(
        name = "Drop-Wave function",
        fn = function(x) {
            1 + cos(12 * sqrt(x[1]^2 + x[2]^2))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5.12, -5.12),
            upper = c(5.12, 5.12),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = -1
    )
}
