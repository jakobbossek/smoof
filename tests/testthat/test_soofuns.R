context("single-objective test functions")

test_that("single-objective test function generators work", {
    # get all relevant methods
    all.methods = unclass(lsf.str(envir = asNamespace("smoof"), all = TRUE))
    all.methods = all.methods[grepl("^make", all.methods)]
    all.methods = Filter(function(fun) exists(fun), all.methods)
    all.methods = setdiff(all.methods, c("makeInternalObjectiveFunction",
      "makeMultiObjectiveFunction", "makeObjectiveFunction",
      "makeSingleObjectiveFunction", "makeBBOBFunction"))
    all.methods = sapply(all.methods, get)
    fun.generators = Filter(function(fun) inherits(fun, "smoof_generator"), all.methods)

    for (fun.generator in fun.generators) {
        fun = try(do.call(fun.generator, list()), silent = TRUE)
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(dimensions = 2L)), silent = TRUE)
        }
        if (inherits(fun, "try-error")) {
            fun = do.call(fun.generator, list(dimensions = 3L, n.objectives = 2L))
        }
        expectIsSnoofFunction(fun, attr(fun.generator, "name"))
        if (hasGlobalOptimum(fun)) {
          expectGlobalOptimum(fun, attr(fun.generator, "name"))
        }
        test.param = ParamHelpers::sampleValues(getParamSet(fun), 1L)
        test.val = fun(test.param)
        expect_true(is.numeric(test.val))
    }
    expect_true(length(filterFunctionsByTags("continuous")) > 0L)
})

test_that("BBOB functions work", {
  fids = 1:24
  iids = c(1, 5, 10, 15, 20)
  dimensions = c(2, 3)
  for (fid in fids) {
    for (iid in iids) {
      for (dimension in dimensions) {
        generator = sprintf("(FID: %i, IID : %i, DIM: %i)", fid, iid, dimension)
        bbob.fn = makeBBOBFunction(dimension = dimension, fid = fid, iid = iid)
        # check vectorized input and output
        if (isVectorized(bbob.fn)) {
          par1 = rep(1, dimension)
          par2 = rep(2, dimension)
          res.seq = c(bbob.fn(par1), bbob.fn(par2))
          res.vec = bbob.fn(cbind(par1, par2))
          expect_true(all(res.seq == res.vec), info = sprintf("Sequential and vectorized input not equal for %s", generator))
        }
        expectIsSnoofFunction(bbob.fn, generator)
        expectGlobalOptimum(bbob.fn, generator)
      }
    }
  }
})
