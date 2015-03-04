#' Alpine01 function
#'
#' Highly multimodal single-objective optimization test function. It is defined
#' as \deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} |\mathbf{x}_i \sin(\mathbf{x}_i) + 0.1\mathbf{x}_i|}
#' with box constraints \eqn{\mathbf{x}_i \in [-10, 10]} for \eqn{i = 1, \ldots, n}.
#'
#' @references Clerc M., The Swarm and the Queen: Towards a Deterministic and Adaptive
#' Particle Swarm Optimization", Congress on Evolutionary Computation, Washington DC, 1999.
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeAlpine01Function = function(dimensions) {
    assertCount(dimensions)
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Alpine01 function", sep = ""),
        fn = function(x) {
            sum(abs(x * sin(x) + 0.1 * x))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(-10, dimensions),
            upper = rep(10, dimensions),
            vector = FALSE
        ),
        tags = c("multimodal"),
        global.opt.params = rep(0, dimensions),
        global.opt.value = 0
    )
}
