#' Holder Table function
#'
#' @template ret_smoof_single
#' @export
makeHolderTableFunction = function() {
    makeSingleObjectiveFunction(
        name = "Holder Table function",
        fn = function(x) {
            -abs(sin(x[1]) * cos(x[2]) * exp(abs(1 - sqrt(x[1]^2 + x[2]^2) / 3.1415)))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        #FIXME: add global opt
        global.opt.params = list("x1" = 8.05502, "x2" = 9.66459),
        global.opt.value = -19.2085
    )
}
