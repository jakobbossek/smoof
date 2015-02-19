#' Step function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeStepFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(0.5, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Step function", sep = ""),
        fn = function(x) {
            sum((floor(x) + 0.5)^2)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-100, dimensions),
            upper = rep(100, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
