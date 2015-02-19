#' Adjiman function
#'
#' @template ret_otf_single
#' @export
makeAdjimanFunction = function() {
    #FIXME: type is multimodal
    makeSingleObjectiveFunction(
        name = "Adjiman function",
        fn = function(x) {
            cos(x[1]) * sin(x[2]) - x[1] / (x[2]^2 + 1)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-1, -1),
            upper = c(2, 1),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 2, "x2" = 0.10578),
        global.opt.value = -2.02181
    )
}
