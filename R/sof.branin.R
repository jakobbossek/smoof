#' Branin function
#'
#' Popular 2-dimensional single-objective test function based on the formula:
#' \deqn{f(\mathbf{x}) = a \left(\mathbf{x}_2 - b \mathbf{x}_1^2 + c \mathbf{x_1} - d\right)^2 + e\left(1 - f\right)\cos(\mathbf{x}_1) + e,}
#' where \eqn{a = 1, b = \frac{5.1}{4\pi^2}, c = \frac{5}{\pi}, d = 6, e = 10} and
#' \eqn{f = \frac{1}{8\pi}}.
#'
#' @template ret_smoof_single
#' @export
makeBraninFunction = function() {
    makeSingleObjectiveFunction(
        name = "Branin function",
        fn = function(x) {
            a = 1
            b = 5.1 / (4 * pi^2)
            c = 5 / pi
            d = 6
            e = 10
            f = 1 / ( 8 * pi)
            return (a * (x[2] - b * x[1]^2 + c * x[1] - d)^2 + e * (1 - f) * cos(x[1]) + e)
        },
        par.set = makeNumericParamSet(
            len = 2L,
            id = "x",
            lower = c(-5, 0),
            upper = c(10, 15),
            vector = FALSE
        ),
        tags = c("continuous", "multimodal"),
        #FIXME: multiple global optima
        # c(-pi, 12.275), c(pi, 2.275), c(3*pi, 2.475)
        #global.opt.params = list("x1" = -3.1415, "x2" = 12.275),
        #global.opt.value = 0
    )
}
