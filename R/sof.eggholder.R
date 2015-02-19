#' Egg Holder function
#'
#' @export
makeEggholderFunction = function() {
    makeSingleObjectiveFunction(
        name = "Eggholder function",
        fn = function(x) {
            #FIXME: this can be extended to arbitrary dimensions
            -(x[2] + 47) * sin(sqrt(abs(x[2] + x[1] * 0.5 + 47))) - x[1] * sin(sqrt(abs(x[1] - (x[2] - 47))))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-512, -512),
            upper = c(512, 512),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 512, "x2" = 404.2319),
        global.opt.value = -959.640662711
    )
}
