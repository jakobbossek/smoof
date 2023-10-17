context("multi-objective test functions")

test_that("(Extended) Bi-objective BBOB functions work", {
  fids = 1:92
  iids = c(1, 5, 10, 15)
  dimensions = c(2, 3)
  
  for (fid in fids) {
    for (iid in iids) {
      for (dimension in dimensions) {
        generator = sprintf("(FID: %i, IID : %i, DIM: %i)", fid, iid, dimension)
        
        fn = makeBiObjBBOBFunction(dimensions = dimension, fid = fid, iid = iid)
        # check vectorized input and output
        
        expectIsSmoofFunction(fn, generator)
        
        test.param = ParamHelpers::sampleValue(getParamSet(fn))
        test.val = fn(test.param)
        expect_true(is.numeric(test.val))
        expect_true(is.logical(shouldBeMinimized(fn)))
        expect_true(all(is.numeric(getUpperBoxConstraints(fn))))
        expect_true(all(is.numeric(getLowerBoxConstraints(fn))))
      }
    }
  }
})
