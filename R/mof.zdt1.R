#' ZDT1 function generator.
#'
#' Builds and returns the two-objective ZDT1 test problem. For \eqn{m} objective it
#' is defined as follows:
#' \deqn{f(\mathbf{x}) = \left(f_1(\mathbf{x}_1), f_2(\mathbf{x})\right)}
#' with
#' \deqn{f_1(\mathbf{x}_1) = \mathbf{x}_1, f_2(\mathbf{x}) = g(\mathbf{x}) h(f_1(\mathbf{x}_1), g(\mathbf{x}))}
#' where
#' \deqn{g(\mathbf{x}) = 1 + \frac{9}{m - 1} \sum_{i = 2}^m \mathbf{x}_i, h(f_1, g) = 1 - \sqrt{\frac{f_1}{g}}}
#' and \eqn{\mathbf{x}_i \in [0,1], i = 1, \ldots, m}
#'
#' @references E. Zitzler, K. Deb, and L. Thiele. Comparison of Multiobjective
#' Evolutionary Algorithms: Empirical Results. Evolutionary Computation, 8(2):173-195, 2000
#'
#' @param dimensions [\code{integer(1)}]\cr
#'   Number of decision variables.
#' @return [\code{smoof_multi_objective_function}]
#' @export
makeZDT1Function = function(dimensions) {
    assertNumber(dimensions, lower = 2L, na.ok = FALSE)
    force(dimensions)

    # define the two-objective ZDT1 function
    fn = function(x) {
        stopifnot(length(x) == dimensions)
        n = length(x)
        f1 = x[1]
        g = 1 + 9 * sum(x[2:n]) / (n - 1)
        h = 1 - sqrt(f1 / g)
        f2 = g * h
        return(c(f1, f2))
    }

    makeMultiObjectiveFunction(
        name = "ZDT1 function",
        description = "Zitzler et al. function 1",
        fn = fn,
        par.set =  makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(0, dimensions),
            upper = rep(1, dimensions),
            vector = FALSE
        ),
        n.objectives = 2L
    )
}
