#' Beale Function
#'
#' @template ret_otf_single
#' @export
makeBealeFunction = function() {
    makeSingleObjectiveFunction(
        name = "Beale Function",
        fn = function(x) {
            a = x[1] * x[2]
            b = a * x[2]
            c = b * x[2]
            (1.5 - x[1] + a)^2 + (2.25 - x[1] + b)^2 + (2.625 - x[1] + c)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-10, -10),
            upper = c(10, 10),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 3, "x2" = 0.5),
        global.opt.value = 0
    )
}
