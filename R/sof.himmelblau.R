#' Himmelblau function
#'
#' @export
makeHimmelblauFunction = function() {
    makeSingleObjectiveFunction(
        name = "Himmelblau function",
        fn = himmelblauCPP,
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5, -5),
            upper = c(5, 5),
            vector = FALSE
        )#,
        #FIXME: multiple global optimums
        # c(-pi, 12.275), c(pi, 2.275), c(3*pi, 2.475)
        #global.opt.params = list("x1" = -3.1415, "x2" = 12.275),
        #global.opt.value = 0
    )
}
