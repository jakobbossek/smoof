context("import from soobench")

test_that("makeSingleObjectiveFunctionFromSOOFunction works as expected", {
    fn = makeSingleObjectiveFunctionFromSOOFunction("ackley", dimensions = 2L)
    expect_true(grepl("Ackley", getName(fn)))
    expect_equal(getNumberOfParameters(fn), 2L)
    expect_true(is.function(fn$fn))
})