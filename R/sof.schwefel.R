#' Schwefel function
#'
#' Highly multimodal test function. The cursial thing about this function is, that
#' the global optimum is far away from the next best local optimum.
#' The function is computed via:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} -\mathbf{x}_i \sin\left(\sqrt(|\mathbf{x}_i|)\right).}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSchwefelFunction = function(dimensions) {
    assertCount(dimensions)
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Schwefel function", sep = ""),
        fn = function(x) {
            sum(-x * sin(sqrt(abs(x))))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-500, dimensions),
            upper = rep(500, dimensions),
            vector = FALSE
        ),
        tags = c("continuous", "multimodal"),
        global.opt.params = rep(420.9687, dimensions),
        global.opt.value = -418.9829 * dimensions
    )
}
