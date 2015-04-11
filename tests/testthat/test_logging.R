context("logging x and y values")

# Helper to check if a logging result has the correct structure
expect_logging_result_structure = function(x) {
  expect_true(is.list(x))
  expect_true(is.data.frame(x$pars))
  expect_true(is.numeric(x$obj.vals))
}

test_that("logging of simple functions works well", {
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
  }
})

test_that("logging of mixed function works well", {
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
