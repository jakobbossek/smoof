#' Branin function
#'
#' @export
makeBraninFunction = function() {
    makeSingleObjectiveFunction(
        name = "Branin function",
        fn = braninCPP,
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
