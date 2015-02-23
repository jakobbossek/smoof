context("makeSingleObjectiveFunction")

test_that("makeSingleObjectiveFunction", {
	name = "Test function"
	description = "Test function description"
	par.set = makeParamSet(
		makeNumericParam("x1", lower = -5, upper = 5),
		makeNumericParam("x2", lower = -5, upper = 5)
	)
	fn = function(x) sum(x^2)
	fn = makeSingleObjectiveFunction(
		name = name,
		description = description,
		fn = fn,
		par.set = par.set,
		global.opt.params = list(x1 = 0, x2 = 0),
		constraint.fn = function(x) {
			sum(x) < 1
		}
	)
	expect_true(issmoofFunction(fn))
	expect_false(isNoisy(fn))
	expect_equal(name, getName(fn))
	expect_equal(description, getDescription(fn))
	expect_output(print(fn), ".*Single.*")
	expect_equal(getNumberOfParameters(fn), 2L)
	expect_is(getParamSet(fn), "ParamSet")
	expect_equal(getNumberOfObjectives(fn), 1L)
	expect_true(hasConstraints(fn))
	expect_true(hasOtherConstraints(fn))
	expect_true(hasBoxConstraints(fn))
	expect_true(hasGlobalOptimum(fn))
	global.optimum = getGlobalOptimum(fn)
	expect_true(!is.null(global.optimum))
	expect_is(global.optimum, "list")
	expect_equal(global.optimum[["param"]][["x1"]], 0)
	expect_equal(global.optimum[["param"]][["x1"]], 0)
	expect_equal(global.optimum[["value"]], 0)

	# global opt params out of bounds
	expect_error(makeSingleObjectiveFunction(name, fn, par.set = par.set, global.opt.params = list(x1 = -10, x2 = 100)))
	# params not named properly
	expect_error(makeSingleObjectiveFunction(name, fn, par.set = par.set, global.opt.params = list(alice = 0, bob = 0)))

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
	expect_is(generateTestFunction(c(num1 = 0)), "smoof_function")
	expect_is(generateTestFunction(0), "smoof_function")
})