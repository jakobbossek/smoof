#' Rastrigin function
#'
#' @export
makeRastriginFunction = function(dimensions) {
    #FIXME: highly multimodal
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Rastrigin function", sep = ""),
        fn = function(x) {
            n = length(x)
            10 * n + sum(x^2 - 10 * cos(6.283185 * x))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-5.12, dimensions),
            upper = rep(5.12, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
