library(methods)
library(devtools)
library(testthat)
library(ggplot2)

load_all(".")

# generatePlotsForREADME
fn1 = makeHimmelblauFunction()
fn2 = makeSphereFunction(dimensions = 2L)
fn3 = makeSchwefelFunction(dimensions = 2L)

png("smoof_funs_example.png", width = 960, height = 300, title = "Examplary functions from our test set")
op = par()
par(mfrow = c(1, 3))
plot3D(fn1, length.out = 50L, contour = TRUE, image = FALSE, facets = TRUE)
plot3D(fn2, length.out = 50L, contour = TRUE, image = FALSE, facets = TRUE)
plot3D(fn3, length.out = 50L, contour = TRUE, image = FALSE, facets = TRUE)
par(op)
dev.off()

