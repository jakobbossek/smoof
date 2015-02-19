#' Styblinksk-Tang function
#'
#' @template arg_dimensions
#' @template ret_otf_single
#' @export
makeStyblinkskTangFunction = function(dimensions) {
    assertCount(dimensions, na.ok = FALSE)
    global.opt.params = as.list(rep(-2.903534, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Styblinksk-Tang function", sep = ""),
        fn = function(x) {
            a = x^2
            b = a^2
            return(0.5 * sum(b - 16 * a + 5 * x))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-5, dimensions),
            upper = rep(5, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = -39.16599 * dimensions
    )
}
