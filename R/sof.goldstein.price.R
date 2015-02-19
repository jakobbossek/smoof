#' Goldstein-Price function
#'
#' @template ret_smoof_single
#' @export
makeGoldsteinPriceFunction = function() {
    makeSingleObjectiveFunction(
        name = "Goldstein Price function",
        fn = function(x) {
            xx1 = x[1]^2
            xx2 = x[2]^2
            xx12 = x[1] * x[2]
            a = 1 + (x[1] + x[2] + 1)^2 * (19 - 14 * x[1] + 3 * xx1 - 14 * x[2] + 6 * xx12 + 3 * xx2)
            b = 30 + (2 * x[1] - 3 * x[2])^2 * (18 - 32 * x[1] + 12 * xx1 + 48 * x[2] - 36 * xx12 + 27 * xx2)
            return (a * b)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-2, -2),
            upper = c(2, 2),
            vector = FALSE
        )
        #FIXME: add global opt
    )
}
