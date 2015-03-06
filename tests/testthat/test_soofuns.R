context("single-objective test functions")

test_that("single-objective test function generators work", {
    expectIsSnoofFunction = function(obj, generator) {
        expect_is(obj, c("smoof_function", "function"), info = "No snoof function generated '%s'.", generator)
    }

    # get all relevant methods
    all.methods = unclass(lsf.str(envir = asNamespace("smoof"), all = TRUE))
    all.methods = all.methods[grepl("^make", all.methods)]
    all.methods = Filter(function(fun) exists(fun), all.methods)
    all.methods = sapply(all.methods, get)
    fun.generators = Filter(function(fun) inherits(fun, "smoof_generator"), all.methods)

    for (fun.generator in fun.generators) {
        fun = try(fun.generator(), silent = TRUE)
        if (inherits(fun, "try-error")) {
            fun = try(fun.generator(dimensions = 2L), silent = TRUE)
        }
        if (inherits(fun, "try-error")) {
            fun = fun.generator(dimensions = 3L, n.objectives = 2L)
        }
        expectIsSnoofFunction(fun, attr(fun.generator, "name"))
        test.param = ParamHelpers::sampleValues(getParamSet(fun), 1L)
        test.val = fun(test.param)
        expect_true(is.numeric(test.val))
    }

    #FIXME: this breaks if done automatically (autolpot.smoof_function not found. WTF?!?)
    #expect_true(length(filterFunctionsByTags("continuous")) > 0L)
})
