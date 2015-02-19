#' Leon function
#'
#' @export
makeLeonFunction = function() {
    makeSingleObjectiveFunction(
        name = "Leon function",
        fn = function(x) {
            100 * (x[2] - x[1]^3)^2 + (x[1] - 1)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 1, "x2" = 1),
        global.opt.value = 0
    )
}
