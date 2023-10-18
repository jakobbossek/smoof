context("(rM)NK-landscapes")

test_that("NK-landscape generator works", {
  N = c(10, 15, 20)

  randomK = function(N) {
    sample(2:7, size = N, replace = TRUE)
  }

  for (n in N) {
    fn = makeNKFunction(n, K = randomK(n))
    x = sampleValue(getParamSet(fn))$x
    value = fn(x)
    expect_true(is.numeric(value))
  }
})

test_that("MNK-landscape generator works", {
  N = c(10, 15, 20)
  M = 2:4
  K_min = 1L
  K_max = 6L

  for (m in M) {
    for (n in N) {
      # random links for each bit and objective
      K = lapply(seq_len(m), function(i) {
        sample(K_min:K_max, size = n, replace = TRUE)
      })
      fn = makeMNKFunction(m, n, K = K)
      x = sampleValue(getParamSet(fn))$x
      value = fn(x)
      expect_true(is.numeric(value))
      expect_true(length(value) == m)
    }
  }
})

test_that("rMNK-landscape generator works", {
  N = c(10, 15, 20)
  M = 2:4

  for (m in M) {
    for (n in N) {
      fn = makeRMNKFunction(rho = runif(1, 0.5, 0.9), m, n, K = 4L)
      x = sampleValue(getParamSet(fn))$x
      value = fn(x)
      expect_true(is.numeric(value))
      expect_true(length(value) == m)
    }
  }
})

test_that("rMNK-landscape works when passing single-objective NK-landscapes", {
  N = c(10, 15, 20)

  for (n in N) {
    fn1 = makeNKFunction(n, K = 4L)
    fn2 = makeNKFunction(n, K = sample(2:7, size = n, replace = TRUE))
    moofn = makeMNKFunction(funs = list(fn1, fn2))

    f = tempfile()
    on.exit(unlink(f))
    exportNKFunction(moofn, f)
    moofn2 = importNKFunction(f)

    for (i in seq_len(20)) {
      x = sampleValue(getParamSet(moofn))$x
      value_1 = moofn(x)
      expect_true(is.numeric(value_1))
      expect_true(length(value_1) == 2L)

      # check if imported and exported functions yield the same values
      value_2 = moofn2(x)
      moofn(x)
      moofn2(x)
      expect_true(sqrt(sum((value_1 - value_2)^2)) < 1e-10)
    }
  }
})
