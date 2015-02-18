#' Bukin function N. 6
#'
#' @export
makeBukinN6Function = function() {
    makeSingleObjectiveFunction(
        name = "Bukin function N.6",
        fn = bukinN6CPP,
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-15, -3),
            upper = c(-5, 3),
            vector = FALSE
        ),
        global.opt.params = list("x1" = -10, "x2" = 1),
        global.opt.value = 0
    )
}
