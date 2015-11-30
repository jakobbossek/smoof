library(methods)
library(devtools)
library(testthat)
library(ggplot2)
library(xtable)

load_all()

fns = getGeneratorFunctions()
for (fn in fns) {
  catf("%s", attr(fn, "name"))
}
catf("%%%%%%%%%%%%%%%%%%%%%%%%%%")
for (fn in Filter(function(x) length(attr(x, "tags")) == 0L, fns)) {
  catf("%s", attr(fn, "name"))
}
n = length(fns)
catf("Overall: %i", n)
n.soo = length(Filter(function(x) length(attr(x, "tags")) > 0L, fns))
catf("SOO: %i", n.soo)
catf("MOO: %i", n - n.soo)

# GET ALL SINGLE-OBJECTIVE FUNS, tags, global opts and values
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gns = getGeneratorFunctions()
# get just the generators with tags assigned, i.e., currently this are the
# single objective non-family generators
gns = Filter(function(x) length(attr(x, "tags")) > 0L, gns)
ds = data.frame()
fun.table = do.call(rbind, lapply(gns, function(gn) {
  data.frame("Function Name" = gsub(" Function", "", attr(gn, "name")), "Tags" = collapse(getTags(gn), sep = ", "))
}))
row.names(fun.table) = NULL

print(xtable(fun.table), include.rownames = FALSE)


# generate and plot self-made sphere function
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fn = makeSingleObjectiveFunction(
  name = "2D-Sphere",
  fn = function(x) x[1]^2 + x[2]^2,
  par.set = makeNumericParamSet(
    len = 2L, id = "x",
    lower = c(-10, -10), upper = c(10, 10),
    vector = TRUE
  ),
  global.opt.param = c(0, 0),
  global.opt.value = 0
)
print(fn)

pdf("2D_sphere_fun.pdf", width = 8, height = 8)
plot(fn, render.levels = TRUE)
dev.off()


# plot some functions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ackley = makeMatyasFunction()
rastrigin = makeRastriginFunction(dimensions = 2L)
himmelblau = makeHimmelblauFunction()
pdf("2D_so_funs.pdf", width = 8, height = 3)
opar = par(mfrow = c(1L, 3L))
plot3D(ackley)
plot3D(rastrigin)
plot3D(himmelblau)
par(opar)
dev.off()
