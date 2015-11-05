context("makeFunctionByName helper")

test_that("makeFunctionByName helper should work as expected", {
  # basic checks
  fn = makeFunctionByName("Ackley Function", dimensions = 2L)
  expect_is(fn, "smoof_function")
  expect_true(grepl("ackley", getName(fn), ignore.case = TRUE))

  # check if 2D function is created with dimensions = 2 and no dimensions
  # attribute at all
  fn = makeFunctionByName("Branin Function", dimensions = 2L)
  expect_is(fn, "smoof_function")
  expect_true(grepl("branin", getName(fn), ignore.case = TRUE))
  fn = makeFunctionByName("Branin Function")
  expect_is(fn, "smoof_function")
  expect_true(grepl("branin", getName(fn), ignore.case = TRUE))
  expect_error(makeFunctionByName("Branin Function", dimensions = 3L))

  # test in combination with filtering
  funs = lapply(as.list(filterFunctionsByTags(c("multimodal", "scalable"))),
    function(fun.name) {
      makeFunctionByName(fun.name, dimensions = 2L)
    }
  )
  expect_list(funs, types = "smoof_function")

  # check if all single objective functions can be generated for 2D
  all.tags = getAvailableTags()
  all.funs.names = unique(unlist(lapply(as.list(all.tags), filterFunctionsByTags)))
  for (fun.name in all.funs.names) {
    sof.fn = makeFunctionByName(fun.name = fun.name, dimensions = 2L)
    expect_is(sof.fn, "smoof_function", info = sprintf("Error generating function '%s'
      with dimensions = 2L.", fun.name))
  }
})
