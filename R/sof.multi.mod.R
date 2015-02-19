#' MultiMod function.
#'
#' @template arg_dimensions
#' @template ret_otf_single
#' @export
makeMultiModFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d MultiMod function", sep = ""),
        fn = function(x) {
            a = abs(x)
            sum(a) * prod(a)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-10, dimensions),
            upper = rep(10, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
