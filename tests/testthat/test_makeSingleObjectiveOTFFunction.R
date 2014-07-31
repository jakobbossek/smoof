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

	pl = autoplot(fn)
	expect_is(pl, "ggplot")
	expect_is(pl, "gg")
})