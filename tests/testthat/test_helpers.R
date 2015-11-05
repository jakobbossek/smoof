context("test some getter and setter functions")

test_that("hasTags works as expected", {
  # check that hasTags works for smoof_functions
  fun.names = as.list(filterFunctionsByTags(c("multimodal")))
  funs = unlist(lapply(fun.names, function(fun.name) {
    makeFunctionByName(fun.name, dimensions = 2L)
  }))

  for (fun in funs) {
    expect_true(hasTags(fun, "multimodal"), info = sprintf("Function '%s' has
      no tag multimodal, but it should have.", getName(fun)))
  }

  # check that hasTags works for smoof_generators
  expect_true(hasTags(makeAckleyFunction, "multimodal"))
  expect_false(hasTags(makeSphereFunction, "multimodal"))

  # check that hasTags works for strings
  expect_true(hasTags("Ackley Function", "multimodal"))
  expect_false(hasTags("Sphere Function", "multimodal"))
})
