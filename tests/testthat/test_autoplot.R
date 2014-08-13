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

test_that("autoplot does not work for certain functions", {
	fn1 = makeSingleObjectiveFunction(
		name = "Function with high dimension",
		fn = function(x) 1,
		par.set = makeNumericParamSet("x", len = 3L)
	)

	fn2 = makeSingleObjectiveFunction(
		name = "Function with unmatching parameters",
		fn = function(x) (as.character(x$disc1) == "a") + as.numeric(x$log1),
		has.simple.signature = FALSE,
		par.set = makeParamSet(
			makeDiscreteParam("disc1", values = letters[1:3]),
			makeLogicalParam("log1")
		)
	)

	library(ggplot2)
	expect_error(autoplot(fn1))
	expect_error(autoplot(fn2))
})

test_that("autoplot functions for 2D mixed functions (one discrete/logical and one numeric)", {
	fn.name = "Modified Sphere function"
	fn = makeSingleObjectiveFunction(
		name = fn.name,
		fn = function(x) x$num1^2 + (as.character(x$disc1) == "a"),
		has.simple.signature = FALSE,
		par.set = makeParamSet(
			makeNumericParam("num1", lower = -2, upper = 2),
			makeDiscreteParam("disc1", values = c("a", "b"))
		)
	)

	library(ggplot2)
	pl = autoplot(fn)
	expect_is(pl, "gg")
	expect_is(pl, "ggplot")
	expect_equal(pl$labels$title, paste("Function:", fn.name))
	expect_null(pl$facet$rows)

	pl = autoplot(fn, use.facets = TRUE)
	expect_is(pl, "gg")
	expect_is(pl, "ggplot")
	expect_equal(pl$labels$title, paste("Function:", fn.name))
	expect_true(!is.null(pl$facet$rows))
})