#' Sphere function
#'
#' Also known as the the \dQuote{De Jong function 1}. Convex, continous function
#' calculated via the formula \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} \mathbf{x}_i.}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeSphereFunction = function(dimensions) {
    assertCount(dimensions)
    global.opt.params = rep(0, dimensions)
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Sphere function", sep = ""),
        fn = function(x) {
            sum(x^2)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-5.12, dimensions),
            upper = rep(5.12, dimensions),
            vector = FALSE
        ),
        tags = c("unimodal", "separable", "convex", "continuous"),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
