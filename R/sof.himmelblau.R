#' Himmelblau Function
#'
#' @export
makeHimmelblauFunction = function() {
    makeSingleObjectiveFunction(
        name = "Himmelblau Function",
        fn = function(x) {
            (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5.12, -5.12),
            upper = c(5.12, 5.12),
            vector = FALSE
        )
        #FIXME: multiple global opt
    )
}
