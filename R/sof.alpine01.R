#' Alpine01 function
#'
#' @export
makeAlpine01Function = function(dimensions) {
    #FIXME: type is convex, unimodal
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Alpine01 function", sep = ""),
        fn = function(x) {
            sum(abs(x * sin(x) + 0.1 * x))
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
