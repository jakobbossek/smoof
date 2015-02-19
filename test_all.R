library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(smoof)
}

test_dir("tests/testthat")
