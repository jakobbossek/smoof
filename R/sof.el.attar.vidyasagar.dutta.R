#' ElAttarVidyasagarDutta function
#'
#' @template ret_smoof_single
#' @export
makeElAttarVidyasagarDuttaFunction = function() {
    makeSingleObjectiveFunction(
        name = "El-Attar-Vidyasagar-Dutta function",
        fn = function(x) {
            (x[1]^2 + x[2] - 10)^2 + (x[1] + x[2]^2 - 7)^2 + (x[1]^2 + x[2]^3 - 1)^2
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-100, -100),
            upper = c(100, 100),
            vector = FALSE
        ),
        global.opt.params = list("x1" = 3.40918683, "x2" = -2.17143304),
        global.opt.value = 1.712780354
    )
}
