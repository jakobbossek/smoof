#' Chichinadze Function
#'
#' @template ret_otf_single
#' @export
makeChichinadzeFunction = function() {
    makeSingleObjectiveFunction(
        name = "Chichinadze Function",
        fn = function(x) {
            x[1]^2 - 12 * x[1] + 11 + 10 * cos(pi * 0.5 * x[1]) + 8 * sin(5 * pi * x[1]) - exp(-0.5 * (x[2] - 0.5)^2) / sqrt(5)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-30, -10),
            upper = c(30, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 5.90133, "x2" = 0.5),
        global.opt.value = -43.3159
    )
}
