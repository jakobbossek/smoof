#' Three-Hump Camel Function
#'
#' @template ret_otf_single
#' @export
makeThreeHumpCamelFunction = function() {
    #FIXME: multimodal, two global optima
    makeSingleObjectiveFunction(
        name = "Three-Hump Camel Function",
        fn = function(x) {
            xx1 = x[1]^2
            xx2 = x[2]^2
            (4 + 2.1 * xx1 + xx1^2 / 3) * xx1 + x[1] * x[2] + (-4 + 4 * xx2) * xx2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-3, -2),
            upper = c(3, 2),
            vector = FALSE
        )
        #FIXME: global opt param(−0.0898, 0.7126) and (0.0898, −0.7126)
        # global opt value -1.0316
    )
}
