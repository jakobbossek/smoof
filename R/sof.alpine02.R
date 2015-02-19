#' Alpine02 function
#'
#' @export
makeAlpine02Function = function(dimensions) {
    #FIXME: type is convex, unimodal
    assertCount(dimensions)
    global.opt.params = as.list(rep(7.917, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Alpine02 function", sep = ""),
        fn = function(x) {
            prod(sqrt(x) * sin(x))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(0, dimensions),
            upper = rep(10, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = -6.1295
    )
}
