context("isMultiobjective/isSingleObjective")

test_that("is{Multi,Single}objective works fine", {
	fn = makeSingleObjectiveFunction(
		name = "Test function",
		fn = function(x) x^2,
		par.set = makeNumericParamSet(id = "x", len = 2L)
	)
	expect_false(isMultiobjective(fn))
	expect_true(isSingleobjective(fn))
})