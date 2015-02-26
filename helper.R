library(methods)
library(devtools)
library(testthat)
library(ggplot2)

load_all(".")

# generatePlotsForREADME
fn1 = makeHimmelblauFunction()
fn2 = makeSphereFunction(dimensions = 2L)
fn3 = makeSchwefelFunction(dimensions = 2L)


pdf("smoof_funs_example.pdf", width = 20, height = 6, title = "Examplary functions from our test set")
op = par()
par(mfrow = c(1, 3))
plot3D(fn1, length.out = 200L, contour = TRUE, image = FALSE, facets = TRUE)
plot3D(fn2, length.out = 200L, contour = TRUE, image = FALSE, facets = TRUE)
plot3D(fn3, length.out = 200L, contour = TRUE, image = FALSE, facets = TRUE)
par(op)
dev.off()

