#' Goldstein-Price function
#'
#' @export
makeGoldsteinPriceFunction = function() {
    makeSingleObjectiveFunction(
        name = "Goldstein Price function",
        fn = goldsteinPriceCPP,
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
