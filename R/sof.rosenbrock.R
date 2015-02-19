#' Rosenbrock function
#'
#' Also known as the second function of De Jong or the (Rosenbrock) banana function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeRosenbrockFunction = function(dimensions) {
    #FIXME: unimodal
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
            lower = rep(-2.048, dimensions),
            upper = rep(2.048, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
