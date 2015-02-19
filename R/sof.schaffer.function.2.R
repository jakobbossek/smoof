#' Schaffer Function N. 2
#'
#' @template ret_otf_single
#' @export
makeSchafferN2Function = function() {
    makeSingleObjectiveFunction(
        name = "Schaffer Function N. 2",
        fn = function(x) {
            a = x[1]^2
            b = x[2]^2
            0.5 + (sin(a - b) - 0.5) / (1 + 0.001 * (a + b))^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-100, -100),
            upper = c(100, 100),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = 0
    )
}
