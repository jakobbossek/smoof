context("makeSingleObjectiveOTFFunction")

test_that("makeSingleObjectiveOTFFunction", {
	name = "Test function"
	fn = makeSingleObjectiveOTFFunction(
		name = name,
		fn = function(x) x^2,
		par.set = makeNumericParamSet(len = 2L)
	)
	expect_true(isOTFFunction(fn))
	expect_false(isNoisy(fn))
	expect_equal(name, getName(fn))
	expect_equal(getNumberOfParameters(fn), 2L)
	expect_is(getParamSet(fn), "ParamSet")
	expect_equal(getNumberOfObjectives(fn), 1L)

	library(ggplot2)
	expect_error(autoplot(fn))
})