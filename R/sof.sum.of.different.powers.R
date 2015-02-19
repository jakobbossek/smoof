#' Sum of different squares function
#'
#' @export
makeSumOfDifferentSquaresFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Sum of different squares function", sep = ""),
        fn = function(x) {
            n = length(x)
            sum(abs(x)^(1:n + 1))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-1, dimensions),
            upper = rep(1, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
