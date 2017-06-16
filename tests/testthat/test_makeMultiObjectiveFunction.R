context("makeMultiObjectiveFunction")

test_that("guessing n.objectives works", {
  # create function without passing n.objectives argument
  fn = makeMultiObjectiveFunction(
    name = "testfun",
    fn = function(x) c(sum(x^2), sum(x^2), sum(x^2)),
    par.set = makeNumericParamSet(len = 3L, lower = -5, upper = 5)
  )

  expectIsSmoofFunction(fn, getID(fn))
  expect_equal(getNumberOfObjectives(fn), 3L)
})
