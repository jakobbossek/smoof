#' Damavandi function
#'
#' @template ret_otf_single
#' @export
makeDamavandiFunction = function() {
    makeSingleObjectiveFunction(
        name = "Damavandi function",
        fn = function(x) {
            t1 = sin(pi * (x[1] - 2)) * sin(pi * (x[2] - 2))
            t2 = pi^2 * (x[1] - 2) * (x[2] - 2)
            a = 1
            b = abs(t1 / t2)^5
            c = 2 + (x[1] - 7)^2 + 2 * (x[2] - 7)^2
            return ((a - b) * c)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(0, 0),
            upper = c(14, 14),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 2, "x2" = 2),
        global.opt.value = 0
    )
}
