#' Get a list of implemented test functions with specific tags.
#'
#' @param tags [\code{character}]\cr
#'   Character vector of tags.
#' @return [\code{character}]
#'   Named vector of function names with the given tags.
#' @export
filterFunctionsByTags = function(tags) {
    assertSubset(tags, choices = getAvailableTags(), empty.ok = FALSE)

    # get all generator methods
    all.methods = unclass(lsf.str(envir = asNamespace("smoof"), all = TRUE))
    fun.generators = all.methods[grep("^make.*Function$", all.methods)]
    fun.generators = setdiff(fun.generators, c("makeSingleObjectiveFunctionFromSOOFunction", "makeSingleObjectiveFunction", "makeMultiObjectiveFunction", "makeInternalObjectiveFunction", "makeObjectiveFunction"))

    # generate 2D version of the function (tags associated to fun and not its generator)
    funs = sapply(fun.generators, function(fun.generator) {
        #FIXME: this is not very elegant
        fun = try(do.call(fun.generator, list()), silent = TRUE)
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(dimensions = 2L)), silent = TRUE)
        }
        if (inherits(fun, "try-error")) {
            fun = do.call(fun.generator, list(dimensions = 2L, n.objectives = 2L))
        }
        return(fun)
    })

    # filter by tags
    filtered.funs = Filter(function(fun) {
        fun.tags = getTags(fun)
        return(BBmisc::isSubset(tags, fun.tags))
    }, funs)

    # cleanup
    names = sapply(filtered.funs, getName)
    names(names) = NULL
    return(names)
}
