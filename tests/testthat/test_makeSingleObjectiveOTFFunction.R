context("makeSingleObjectiveOTFFunction")

test_that("makeSingleObjectiveOTFFunction", {
	fn = makeSingleObjectiveOTFFunction(
		name = "Test function",
		fn = function(x) x^2,
		par.set = makeNumericParamSet(len = 2L)
	)
	expect_true(isOTFFunction(fn))
	expect_false(isNoisy(fn))
})