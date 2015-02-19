#' Schwefel function
#'
#' @export
makeSchwefelFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(420.9687, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Schwefel function", sep = ""),
        fn = function(x) {
            sum(x * sin(sqrt(abs(x))))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-500, dimensions),
            upper = rep(500, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = -418.9829 * dimensions
    )
}
