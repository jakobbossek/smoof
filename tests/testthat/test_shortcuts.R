context("shortcut functions, i.e., snof and smof")

test_that("snof works as expected", {
  fn = snof(n.pars = 10L, fn = function(x) sum(x^2))
  fn.baseline = makeSphereFunction(dimensions = 10L)
  expectIsSmoofFunction(fn, getID(fn))
  rnds = matrix(runif(10L * 10L), nrow = 10L)
  expect_true(all.equal(apply(rnds, 1L, fn), apply(rnds, 1L, fn.baseline)))
})
