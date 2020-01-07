context("shortcut functions, i.e., snof and smof")

test_that("snof works as expected", {
  fn = snof(par.len = 10L, fn = function(x) sum(x^2))
  fn.baseline = makeSphereFunction(dimensions = 10L)
  expectIsSmoofFunction(fn, getID(fn))
  rnds = matrix(runif(10L * 10L), nrow = 10L)
  expect_true(all.equal(apply(rnds, 1L, fn), apply(rnds, 1L, fn.baseline)))
})

test_that("mnof works as expected", {
  fn = mnof(par.len = 3L, fn = function(x) c(sum(x^2), sum(x^2)), n.objectives = 2L)
  fn.baseline = makeBiSphereFunction(dimensions = 3L)
  expectIsSmoofFunction(fn, getID(fn))

  # now check function values
  rnds = matrix(runif(3 * 10L), nrow = 10L)
  vals.fn = apply(rnds, 1L, fn)
  vals.fn.baseline = apply(rnds, 1L, fn.baseline)
  expect_true(all.equal(vals.fn, vals.fn.baseline))
})
