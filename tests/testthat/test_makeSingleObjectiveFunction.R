context("makeSingleObjectiveFunction")

test_that("makeSingleObjectiveFunction", {
	name = "Test function"
	fn = makeSingleObjectiveFunction(
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
	expect_false(hasConstraints(fn))

	library(ggplot2)
	expect_error(autoplot(fn))
})

test_that("global optimum is provided properly", {

	generateTestFunction = function(global.opt.params) {
		fn = makeSingleObjectiveFunction(
			name = "My test function",
			fn = function(x) x^2,
			par.set = makeParamSet(
				makeNumericParam("num1", lower = -10, upper = 10)
			),
			global.opt.params = global.opt.params
		)
	}
	
	expect_error(generateTestFunction(list(num1 = 100)))
	expect_is(generateTestFunction(list(num1 = 0)), "otf_function")
})