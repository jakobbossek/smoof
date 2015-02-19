#' Deflected Corrugated Spring function
#'
#' @template arg_dimensions
#FIXME: what do these parameters do?
#' @param K [\code{numeric(1)}]\cr
#'   Parameter.
#' @param alpha [\code{numeric(1)}]\cr
#'   Parameter.
#' @template ret_smoof_single
#' @export
makeDeflectedCorrugatedSpringFunction = function(dimensions, K = 5, alpha = 5) {
    #FIXME: type is convex, unimodal
    assertCount(dimensions)
    #FIXME: are there any restrictions on K and alpha?
    assertNumber(K, na.ok = FALSE)
    assertNumber(alpha, na.ok = FALSE)

    force(K)
    force(alpha)

    global.opt.params = as.list(rep(alpha, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Deflected Corrugated Spring function", sep = ""),
        fn = function(x) {
            a = (x - alpha)^2
            0.1 * sum(a - cos(K * sqrt(sum(a))))
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(0, dimensions),
            upper = rep(2 * alpha, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
