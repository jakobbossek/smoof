#' Rosenbrock function
#'
#' @export
makeRosenbrockFunction = function(dimensions) {
    assertCount(dimensions, na.ok = FALSE)
    global.opt.params = as.list(rep(1, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Rosenbrock function", sep = ""),
        fn = function(x) {
            i = 1:(length(x) - 1L)
            sum(100 * (x[i + 1] - x[i]^2)^2 + (x[i] - 1)^2)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-5, dimensions),
            upper = rep(10, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
