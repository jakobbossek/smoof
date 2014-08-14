context("makeSingleObjectiveFunction")

test_that("makeSingleObjectiveFunction", {
	name = "Test function"
	fn = makeSingleObjectiveFunction(
		name = name,
		fn = function(x) sum(x^2),
		par.set = makeParamSet(
			makeNumericParam("x1", lower = -5, upper = 5),
			makeNumericParam("x2", lower = -5, upper = 5)
		),
		global.opt.params = list(x1 = 0, x2 = 0)
	)
	expect_true(isOTFFunction(fn))
	expect_false(isNoisy(fn))
	expect_equal(name, getName(fn))
	expect_equal(getNumberOfParameters(fn), 2L)
	expect_is(getParamSet(fn), "ParamSet")
	expect_equal(getNumberOfObjectives(fn), 1L)
	expect_true(hasConstraints(fn))
	expect_true(hasGlobalOptimum(fn))
	global.optimum = getGlobalOptimum(fn)
	expect_is(global.optimum, "list")
	expect_equal(global.optimum[["param"]][["x1"]], 0)
	expect_equal(global.optimum[["param"]][["x1"]], 0)
	expect_equal(global.optimum[["value"]], 0)
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