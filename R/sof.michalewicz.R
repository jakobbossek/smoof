#' Michalewicz function
#'
#' @export
makeMichalewiczFunction = function(m = 5) {
    assertNumber(m, na.ok = FALSE)

    force(5)

    makeSingleObjectiveFunction(
        name = "Michalewicz function",
        fn = function(x) {
            #FIXME: arbitrary dimensions http://www.geocities.ws/eadorio/mvf.pdf
            e = 2 * m
            (-1) * (sin(x[1]) * sin(x[1]^2 / pi)^e + sin(x[2]) * sin(2 * x[2]^2 / pi)^e)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(0, 0),
            upper = c(pi, pi),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 0, "x2" = 0),
        global.opt.value = -1.8013
    )
}
