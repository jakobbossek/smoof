context("single-objective test functions")

test_that("single-objective test function generators work", {
    # get all relevant methods
    all.methods = unclass(lsf.str(envir = asNamespace("smoof"), all = TRUE))
    all.methods = all.methods[grepl("^make", all.methods)]
    all.methods = Filter(function(fun) exists(fun), all.methods)
    all.methods = setdiff(all.methods, c(
      "makeNKFunction",
      "makeMNKFunction",
      "makeRMNKFunction",
      "makeNKFunctionInternal",
      "makeRMNKFunctionInternal",
      "makeRMNKFunctionInternalFromFunctions",
      "makeMultiObjectiveFunction", "makeObjectiveFunction",
      "makeSingleObjectiveFunction", "makeBBOBFunction",
      "makeShekelFunction",
      "makeUFFunction", "makeUFParamSet", "makeMPM2Function",
      "makeGOMOPFunction", paste0("makeMOP", 1:7, "Function"), "makeHartmannFunction"))
    all.methods = sapply(all.methods, get)
    fun.generators = Filter(function(fun) inherits(fun, "smoof_generator"), all.methods)

    for (fun.generator in fun.generators) {
        fun = try(do.call(fun.generator, list()), silent = TRUE)
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(dimensions = 2L)), silent = TRUE)
        }
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(dimensions = 3L, n.objectives = 2L)), silent = TRUE)
        }
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(dimensions = 3L, fid = 2L, iid = 1L)), silent = TRUE) #BBOBFunction
        }
        if (inherits(fun, "try-error")) {
            fun = try(do.call(fun.generator, list(n.objectives = 2L, k = 2L, l = 2L))) #WFG
        }
        expectIsSmoofFunction(fun, attr(fun.generator, "name"))
        if (hasGlobalOptimum(fun)) {
          expectGlobalOptimum(fun, attr(fun.generator, "name"))
        }
        test.param = ParamHelpers::sampleValue(getParamSet(fun))
        test.val = fun(test.param)
        expect_true(is.numeric(test.val))
        expect_true(is.logical(shouldBeMinimized(fun)))
        expect_true(all(is.numeric(getUpperBoxConstraints(fun))))
        expect_true(all(is.numeric(getLowerBoxConstraints(fun))))
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
        bbob.fn = makeBBOBFunction(dimensions = dimension, fid = fid, iid = iid)
        # check vectorized input and output
        if (isVectorized(bbob.fn)) {
          par1 = rep(1, dimension)
          par2 = rep(2, dimension)
          res.seq = c(bbob.fn(par1), bbob.fn(par2))
          res.vec = bbob.fn(cbind(par1, par2))
          expect_true(all(res.seq == res.vec), info = sprintf("Sequential and vectorized input not equal for %s", generator))
        }
        expectIsSmoofFunction(bbob.fn, generator)
        expectGlobalOptimum(bbob.fn, generator)
      }
    }
  }
})

test_that("Multiple peaks model 2 (MPM2) functions work", {
  # mpm2 only available for unix systems
  if (BBmisc::isUnix()) {
    for (dimension in c(1, 2, 5, 10)) {
      for (n.peaks in c(2, 5, 10)) {
        for (topology in c("funnel", "random")) {
          for (rotated in c(TRUE, FALSE)) {
            for (peak.shape in c("ellipse", "sphere")) {
              fnp = makeMPM2Function(n.peaks = n.peaks, dimensions = dimension,
                                     topology = topology, seed = 123, rotated = rotated,
                                     peak.shape = peak.shape, evaluation.env = "Python")
              fnr = makeMPM2Function(n.peaks = n.peaks, dimensions = dimension,
                                     topology = topology, seed = 123, rotated = rotated,
                                     peak.shape = peak.shape, evaluation.env = "R")
              
              # confirm that both evaluation environments can be created
              expect_is(fnp, "smoof_single_objective_function")
              yp = fnp(rep(0.1, dimension))
              expect_true(is.numeric(yp))
              
              expect_is(fnr, "smoof_single_objective_function")
              yr = fnr(rep(0.1, dimension))
              expect_true(is.numeric(yr))
              
              # confirm that results are identical between evaluation environments
              expect_equal(yp, yr)
              
              # confirm vectorization works as expected
              expect_true(isVectorized(fnr))
              expect_true(isVectorized(fnp))
              
              par1 = rep(0.1, dimension)
              par2 = rep(0.2, dimension)
              
              res.seq = c(fnr(par1), fnr(par2))
              res.vec = fnr(cbind(par1, par2))
              expect_true(all(res.seq == res.vec),
                          info = sprintf("Sequential and vectorized input not equal for %s (R)", getID(fnr)))
              
              res.seq = c(fnr(par1), fnr(par2))
              res.vec = fnr(cbind(par1, par2))
              expect_true(all(res.seq == res.vec),
                          info = sprintf("Sequential and vectorized input not equal for %s (Python)", getID(fnr)))
              
            }
          }
        }
      }
    }
  }
})

test_that("CEC 2009 functions work", {
  ids = 1:10
  dimensions = c(3, 5, 10)
  for (id in ids) {
    for (dimension in dimensions) {
      fn = makeUFFunction(dimensions = dimension, id = id)
      param = sampleValue(getParamSet(fn))
      value = fn(param)
      expect_true(is.numeric(value))
      expect_equal(length(value), getNumberOfObjectives(fn),
        info = sprintf("Length of objective vector is wrong!\nExpected %i, but got %i for
        dimension %i and UF%i", length(value), getNumberOfObjectives(fn),
        dimension, id))
    }
  }
})

test_that("NK-landscape generator works", {
  N = c(10, 15, 20)

  randomK = function(N) {
    sample(2:7, size = N, replace = TRUE)
  }

  for (n in N) {
    fn = makeNKFunction(n, K = randomK(n))
    x = sampleValue(getParamSet(fn))$x
    value = fn(x)
    expect_true(is.numeric(value))
  }
})

test_that("rMNK-landscape generator works", {
  N = c(10, 15, 20)
  M = 2:4

  for (m in M) {
    for (n in N) {
      fn = makeRMNKFunction(rho = runif(1, 0.5, 0.9), m, n, K = 4L)
      x = sampleValue(getParamSet(fn))$x
      value = fn(x)
      expect_true(is.numeric(value))
      expect_true(length(value) == m)
    }
  }
})

test_that("rMNK-landscape works when passing single-objective NK-landscapes", {
  N = c(10, 15, 20)

  for (n in N) {
    fn1 = makeNKFunction(n, K = 4L)
    fn2 = makeNKFunction(n, K = sample(2:7, size = n, replace = TRUE))
    moofn = makeMNKFunction(funs = list(fn1, fn2))
    x = sampleValue(getParamSet(moofn))$x
    value = moofn(x)
    expect_true(is.numeric(value))
    expect_true(length(value) == 2L)
  }
})
