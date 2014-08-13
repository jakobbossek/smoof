context("autoplot function")

test_that("autoplot functions for 1D numeric functions works as expected", {
	fn = makeSingleObjectiveFunction(
		name = "Test function",
		fn = function(x) sum(x^2),
		par.set = makeNumericParamSet("x", len = 1L, lower = -2, upper = 2)
	)

	library(ggplot2)
	pl = autoplot(fn)
	expect_is(pl, "gg")
	expect_is(pl, "ggplot")
	expect_equal(pl$labels$title, "Function: Test function")
	expect_equal(pl$labels$x, "x")
	expect_equal(pl$labels$y, "y")
})

test_that("autoplot does not work for functions with more >= 3 parameters", {
	fn = makeSingleObjectiveFunction(
		name = "Test function",
		fn = function(x) 1,
		par.set = makeNumericParamSet("x", len = 3L)
	)

	library(ggplot2)
	expect_error(autoplot(fn))
})