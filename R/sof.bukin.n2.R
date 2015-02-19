#' Bukin function N. 2
#'
#' @template ret_otf_single
#' @export
makeBukinN2Function = function() {
    makeSingleObjectiveFunction(
        name = "Bukin function N. 2",
        fn = function(x) {
            100 * (x[2] - 0.01 * x[1]^2 + 1) + 0.01 * (x[1] + 10)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-15, -3),
            upper = c(-5, 3),
            vector = FALSE
        ),
        global.opt.params = list("x1" = -10, "x2" = 0),
        global.opt.value = 0
    )
}
