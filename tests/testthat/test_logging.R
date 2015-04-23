context("logging x and y values")

# Helper to check if a logging result has the correct structure
expect_logging_result_structure = function(x) {
  expect_true(is.list(x))
  expect_true(is.data.frame(x$pars))
  expect_true(is.numeric(x$obj.vals))
}

test_that("logging for functions with matrix input works well", {
  fn = makeBBOBFunction(fid = 1L, iid = 1L, dimension = 10L)
  fn = addLoggingWrapper(fn, logg.x = TRUE)
  fn(matrix(runif(10L * 10L), ncol = 10L))

  res = getLoggedValues(fn, compact = TRUE)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 10L)
  expect_equal(ncol(res), 10L + 1L) # dim plus y
})

test_that("logging for simple functions works well", {
  # generate Sphere function
  for (dimension in c(1L, 2L, 5L, 10L)) {
    fn = makeSphereFunction(dimension = dimension)
    par.ids = getParamIds(smoof::getParamSet(fn), with.nr = TRUE, repeated = TRUE)

    # add logger for both x and y values
    fn = addLoggingWrapper(fn, logg.x = TRUE)

    # now apply some evaluations
    fn(runif(dimension))

    # check for logged vals
    res = getLoggedValues(fn)

    expect_logging_result_structure(res)
    expect_equal(nrow(res$pars), 1L)
    expect_equal(length(res$obj.vals), 1L)

    for (i in seq(10L)) {
      fn(runif(dimension))
    }
    res = getLoggedValues(fn)

    expect_logging_result_structure(res)
    expect_equal(nrow(res$pars), 11L)
    expect_equal(length(res$obj.vals), 11L)

    # check "compact" logging result
    res = getLoggedValues(fn, compact = TRUE)
    expect_true(is.data.frame(res))
    expect_equal(nrow(res), 11L)
    expect_equal(ncol(res), dimension + 1L) # dim plus the single objective value
  }
})

test_that("logging for mixed function works well", {
  # define a mixed function with three parameters
  fn = makeSingleObjectiveFunction(
    name = "Test",
    fn = function(x) {
      if (x$disc == "a") {
        return(x$x1 + x$x2)
      }
      return(x$x1 + x$x2 + 1L)
    },
    par.set = makeParamSet(
      makeDiscreteParam("disc", values = letters[1:2]),
      makeNumericParam("x1"),
      makeNumericParam("x2", lower = 0, upper = 10)
    ),
    has.simple.signature = FALSE
  )

  # add logger
  fn = addLoggingWrapper(fn, logg.x = TRUE)

  test.df = data.frame(
    disc = c("a", "a", "b"),
    x1 = c(0, 0, 1),
    x2 = c(0, 0, 1)
  )
  obj.vals = c(0, 0, 3)

  for (i in 1:nrow(test.df)) {
    fn(dfRowToList(test.df, i, par.set = smoof::getParamSet(fn)))
  }

  res = getLoggedValues(fn)
  expect_logging_result_structure(res)
  expect_equal(test.df, res$pars)
  expect_true(all(obj.vals == res$obj.vals))
})

test_that("nesting of wrappers works well", {
  fn = makeSphereFunction(2L)
  fn = addCountingWrapper(fn)
  fn = addLoggingWrapper(fn, logg.x = TRUE)

  # evaluate 10 times
  n.evals = 10L
  for (i in seq(n.evals)) {
    fn(runif(2))
  }

  # should be a wrapped function now
  expect_true(isWrappedSmoofFunction(fn))
  expect_equal(getNumberOfEvaluations(fn), n.evals)
  expect_logging_result_structure(getLoggedValues(fn))

  # now unwrap step by step
  fn2 = getWrappedFunction(fn)
  expect_true(isWrappedSmoofFunction(fn2))
  expect_equal(getNumberOfEvaluations(fn), n.evals)
  fn2 = getWrappedFunction(fn2)
  expect_false(isWrappedSmoofFunction(fn2))
  expect_true(isSmoofFunction(fn2))

  # now unwrap completely
  fn2 = getWrappedFunction(fn, deepest = TRUE)
  expect_false(isWrappedSmoofFunction(fn2))
  expect_true(isSmoofFunction(fn2))
})
