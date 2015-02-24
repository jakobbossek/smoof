#' Easom Function
#'
#' Unimodal function with its global optimum in the center of the search space.
#' The attraction area of the global optimum is very small in relation to the
#' search space:
#' \deqn{f(\mathbf{x}) = -\cos(\mathbf{x}_1)\cos(\mathbf{x}_2)\exp\left(-\left((\mathbf{x}_1 - \pi)^2 + (\mathbf{x}_2 - pi)^2\right)\right).}
#'
#' @template ret_smoof_single
#' @export
makeEasomFunction = function() {
    makeSingleObjectiveFunction(
        name = "Easom Function",
        fn = function(x) {
            -cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-100, -100),
            upper = c(100, 100),
            vector = FALSE
        ),
        tags = c("continuous", "unimodal"),
        global.opt.params = c(pi, pi),
        global.opt.value = -1
    )
}
