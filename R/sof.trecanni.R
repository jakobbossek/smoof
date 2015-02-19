#' Trecanni Function
#'
#' @export
makeTrecanniFunction = function() {
    makeSingleObjectiveFunction(
        name = "Trecanni Function",
        fn = function(x) {
            x[1]^4 + 4 * (x[1]^3 + x[1]^2) + x[2]^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5, -5),
            upper = c(5, 5),
            vector = FALSE
        ),
        #FIXME: global opts at (0,0) and (-2,0)
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = -1.9133
    )
}
