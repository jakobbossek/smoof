#' Schaffer Function N. 4
#'
#' @template ret_otf_single
#' @export
makeSchafferN4Function = function() {
    makeSingleObjectiveFunction(
        name = "Schaffer Function N. 4",
        fn = function(x) {
            a = x[1]^2
            b = x[2]^2
            0.5 + (cos(sin(abs(a - b))) - 0.5) / (1 + 0.001 * (a + b))^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-100, -100),
            upper = c(100, 100),
            vector = FALSE
        )
        #FIXME: global opt?
    )
}
