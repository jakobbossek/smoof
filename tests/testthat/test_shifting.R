test_that("Origin moves to new location", {
            fn = makeBBOBFunction(fid=1L, iid=1L, dim=2L)
            shifted_fn = moveOrigin(fn, shift=c(2, 3))
            expect_equal(shifted_fn(c(2, 3)), fn(c(0, 0)))
})

test_that("Optimum location is correct", {
            fn = makeBBOBFunction(fid=1L, iid=1L, dim=10L)
            shifted_fn = moveOrigin(fn, shift=runif(10))

            opt = getGlobalOptimum(shifted_fn)
            expect_equal(shifted_fn(opt$param), opt$value)
})

test_that("Optimum value does not change", {
            fn = makeBBOBFunction(fid=1L, iid=1L, dim=10L)
            shifted_fn = moveOrigin(fn, shift=runif(10))

            opt = getGlobalOptimum(fn)
            shifted_opt = getGlobalOptimum(shifted_fn)
            expect_equal(shifted_opt$value, opt$value)
})
