#' Ackley function.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeAckleyFunction = function(dimensions) {
    #FIXME: multimodal
    assertCount(dimensions, na.ok = FALSE)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Ackley function", sep = ""),
        fn = function(x) {
            n = length(x)
            a = 20
            b = 0.2
            c = 2 * pi
            d = mean(x^2)
            e = mean(cos(c * x))
            -a * exp(-b * sqrt((1 / n) * d)) - exp((1 / n) * e) + a + exp(1)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = -32.768,
            upper = 32.768,
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0L
    )
}
