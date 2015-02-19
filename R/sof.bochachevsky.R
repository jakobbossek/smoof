#' Bochachevsky Function
#'
#' @export
makeBochachevskyFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = "Bochachevsky Function",
        fn = function(x) {
            i = 1:(length(x) - 1)
            sum(x[i] + 2 * x[i + 1]^2 - 0.3 * cos(3 * pi * x[i]) - 0.4 * cos(4 * pi * x[i + 1]) + 0.7)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-15, dimensions),
            upper = rep(15, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
