#' Booth Function
#'
#' @template ret_smoof_single
#' @export
makeBoothFunction = function() {
    makeSingleObjectiveFunction(
        name = "Booth Function",
        fn = function(x) {
            (x[1] + 2 * x[2] - 7)^2 + (2 * x[1] + x[2] - 5)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 1, "x2" = 3),
        global.opt.value = 0
    )
}
