#' Katsuura function
#'
#' @template arg_dimensions
#FIXME: parameter meaning
#' @param d [\code{integer(1)}]\cr
#'   Parameter.
#' @template ret_otf_single
# @export
#FIXME: do not export for now. There seems to be a problem
makeKatsuuraFunction = function(dimensions, d = 32) {
    #FIXME: type is convex, unimodal
    assertCount(dimensions, na.ok = FALSE)
    assertCount(d, na.ok = FALSE)

    force(d)

    global.opt.params = as.list(rep(0, dimensions))
    names(global.opt.params) = paste("x", seq(dimensions), sep = "")
    makeSingleObjectiveFunction(
        name = paste(dimensions, "-d Katsuura function", sep = ""),
        fn = function(x) {
            r = 1
            #FIXME: quick and dirty implementation. Make it more R like.
            #FIXME: see http://www.geocities.ws/eadorio/mvf.pdf for another definition -.-
            for (i in 1:(length(x) - 1)) {
                t = 0
                for (k in 1:d) {
                    t = t + floor(2^k * x[i]) * 2^(-k)
                }
                r = r  + (1 + (i + 1) * t)
            }
            return(r)
        },
        par.set = makeNumericParamSet(
            len = dimensions,
            id = "x",
            lower = rep(0, dimensions),
            upper = rep(1, dimensions),
            vector = FALSE
        ),
        global.opt.params = global.opt.params,
        global.opt.value = 1
    )
}
