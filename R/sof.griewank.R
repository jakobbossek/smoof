#' Griewank function
#'
#' @template arg_dimensions
#' @template ret_smoof_single
#' @export
makeGriewankFunction = function(dimensions) {
    #FIMXE: (highly) multimodal
    assertCount(dimensions)
    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
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
        global.opt.params = global.opt.params,
        global.opt.value = 0
    )
}
