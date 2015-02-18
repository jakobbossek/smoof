#' Ackley function.
#'
#' @param dimension [\code{dimension}]\cr
#'   Number of parameters.
#' @export
makeAckleyFunction = function(dimension) {
    assertCount(dimension, na.ok = FALSE)
    global.opt.params = as.list(rep(0, dimension))
    names(global.opt.params) = paste("x", seq(dimension), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimension, "-d Ackley function", sep = ""),
        fn = ackleyCPP,
        par.set = makeNumericParamSet(
            len = dimension,
            id = "x",
            lower = -32.768,
            upper = 32.768,
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0L
    )
}
