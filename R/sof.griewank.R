#' Griewank function
#'
#' Highly multimodal function with a lot of regularly distributed local minima.
#' The corresponding formula is:
#' \deqn{f(\mathbf{x}) = \sum_{i=1}^{n} \frac{\mathbf{x}_i^2}{4000} - \prod_{i=1}^{n} \cos\left(\frac{\mathbf{x}_i}{\sqrt{i}}\right) + 1.}
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeGriewankFunction = function(dimensions) {
    assertCount(dimensions)
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Griewank function", sep = ""),
        fn = function(x) {
            a = sum(x^2) / 4000
            b = prod(cos(x / sqrt(1:length(x))))
            return(a - b + 1)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-600, dimensions),
            upper = rep(600, dimensions),
            vector = FALSE
        ),
        tags = c("continuous", "multimodal"),
        global.opt.params = rep(0, dimensions),
        global.opt.value = 0
    )
}
