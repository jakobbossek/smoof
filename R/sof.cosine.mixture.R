#' CosineMixture Function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeCosineMixtureFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = "CosineMixture Function",
        fn = function(x) {
            a = -0.1 * sum(cos(5 * pi * x))
            b = sum(x^2)
            return (a - b)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = c(-1, -1),
            upper = c(1, 1),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = -0.1 * dimensions
    )
}
