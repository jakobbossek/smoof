#' Egg crate function
#'
#' @template ret_smoof_single
#' @export
makeEggCrateFunction = function() {
    makeSingleObjectiveFunction(
        name = "Egg Crate function",
        fn = function(x) {
            x[1]^2 + x[2]^2 + 25 * (sin(x[1])^2 + sin(x[2])^2)
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
