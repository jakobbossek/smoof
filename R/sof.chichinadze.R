#' Chichinadze Function
#'
#' @export
makeChichinadzeFunction = function() {
    makeSingleObjectiveFunction(
        name = "Chichinadze Function",
        fn = function(x) {
            x[1]^2 - 12 * x[1] + 8 * sin(2.5 * pi * x[1]) + 10 * cos(0.5 * x[1]) + 11 - 0.2 * sqrt(5) / exp(0.5 * (x[2] - 0.5)^2)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-30, -30),
            upper = c(30, 30),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 6.189866586965680, "x2" = 0.5),
        global.opt.value = -42.94438701899098
    )
}
