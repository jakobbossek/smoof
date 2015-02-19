#' Dixon-Price function
#'
#' @template arg_dimensions
#' @template ret_otf_single
#' @export
makeDixonPriceFunction = function(dimensions) {
    assertCount(dimensions)
    i = 1:dimensions
    global.opt.params = as.list(2^((-1) * (2^i - 2) / 2^i))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Dixon-Price function", sep = ""),
        fn = function(x) {
            a = (x[1] - 1)^2
            i = 2:length(x)
            b = sum(i * (2 * x[i]^2 - x[i - 1])^2)
            return(a + b)
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
