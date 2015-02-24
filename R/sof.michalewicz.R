#' Michalewicz function
#'
#' Highly multimodal single-objective test function with \eqn{n!} local minima
#' with the formula:
#' \deqn{f(\mathbf{x}) = -\sum_{i=1}^{n} \sin(\mathbf{x}_i) \cdot \left(\sin\left(\frac{i \cdot \mathbf{x}_i}{\pi}\right)\right)^{2m}.}
#'
#' @template arg_dimensions
#' @param m [\code{integer(1)}]\cr
#'   \dQuote{Steepness} parameter.
#' @template ret_smoof_single
#' @export
makeMichalewiczFunction = function(dimensions, m = 5) {
    assertCount(dimensions, na.ok = FALSE)
    assertNumber(m, na.ok = FALSE)
    force(m)
    makeSingleObjectiveFunction(
        name = "Michalewicz function",
        fn = function(x) {
            i = 1:length(x)
            (-1) * sum(sin(x) * (sin((i * x^2) / pi)^(2 * m)))
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(0, 0),
            upper = c(pi, pi),
            vector = FALSE
        ),
        tags = c("continuous", "multimodal")
        #FIXME: multiple global optima
        #global.opt.params = rep(0, dimensions),
        #global.opt.value = -1.8013
    )
}
