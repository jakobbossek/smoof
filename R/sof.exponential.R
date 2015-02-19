#' Exponential function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeExponentialFunction = function(dimensions) {
    #FIXME: type is convex, unimodal
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Exponential function", sep = ""),
        fn = function(x) {
            -exp(-0.5 * sum(x^2))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-1, dimensions),
            upper = rep(1, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = -1
    )
}
