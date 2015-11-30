context("makeFunctionsByName helper")

test_that("makeFunctionsByName helper should work as expected", {
  # basic checks
  fn = makeFunctionsByName("Ackley", dimensions = 2L)
  expect_is(fn, "smoof_function")
  expect_true(grepl("ackley", getName(fn), ignore.case = TRUE))

  # check if 2D function is created with dimensions = 2 and no dimensions
  # attribute at all
  fn = makeFunctionsByName("BraninRCOS", dimensions = 2L)
  expect_is(fn, "smoof_function")
  expect_true(grepl("branin", getName(fn), ignore.case = TRUE))
  fn = makeFunctionsByName("BraninRCOS")
  expect_is(fn, "smoof_function")
  expect_true(grepl("branin", getName(fn), ignore.case = TRUE))
  expect_error(makeFunctionsByName("Branin", dimensions = 3L))

  # test in combination with filtering
  funs = lapply(filterFunctionsByTags(c("multimodal", "scalable")),
    function(fun.name) {
      makeFunctionsByName(fun.name, dimensions = 2L)
    }
  )
  expect_list(funs, types = "smoof_function")

  # check if all single objective functions can be generated for 2D
  all.tags = getAvailableTags()
  all.funs.names = unique(unlist(lapply(as.list(all.tags), filterFunctionsByTags)))
  for (fun.name in all.funs.names) {
    sof.fn = makeFunctionsByName(fun.name = fun.name, dimensions = 2L)
    expect_is(sof.fn, "smoof_function", info = sprintf("Error generating function '%s'
      with dimensions = 2L.", fun.name))
  }
})

test_that("makeFunctionsByName works for list of generator names", {
  fun.names = filterFunctionsByTags(c("multimodal", "scalable"))
  funs = makeFunctionsByName(as.list(fun.names), dimensions = 2L)
  is.smoof = sapply(funs, isSmoofFunction)
  expect_true(all(is.smoof))
})
