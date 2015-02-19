#' Branin function
#'
#' @export
makeBraninFunction = function() {
    makeSingleObjectiveFunction(
        name = "Branin function",
        fn = function(x) {
            a = 1
            b = 5.1 / (4 * pi^2)
            c = 5 / pi
            d = 6
            e = 10
            f = 1 / ( 8 * pi)
            return (a * (x[2] - b * x[1]^2 + c * x[1] - d)^2 + e * (1 - f) * cos(x[1]) + e)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5, 0),
            upper = c(10, 15),
            vector = FALSE
        )#,
        #FIXME: multiple global optimums
        # c(-pi, 12.275), c(pi, 2.275), c(3*pi, 2.475)
        #global.opt.params = list("x1" = -3.1415, "x2" = 12.275),
        #global.opt.value = 0
    )
}
