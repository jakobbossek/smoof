context("isMultiobjective/isSingleObjective")

test_that("is{Multi,Single}objective works fine", {
	fn = makeSingleObjectiveFunction(
		name = "Single-objective function",
		fn = function(x) x^2,
		par.set = makeNumericParamSet(id = "x", len = 2L)
	)
	expect_false(isMultiobjective(fn))
	expect_true(isSingleobjective(fn))

	fn = makeMultiObjectiveFunction(
		name = "Multi-objective function",
		fn = function(x) c(x^2, exp(x)),
		n.objectives = 2L,
		par.set = makeNumericParamSet(id = "x", len = 1L)
	)

	expect_true(isMultiobjective(fn))
	expect_false(isSingleobjective(fn))
	expect_equal(getNumberOfObjectives(fn), 2L)
})