remotes::install_github("jakobbossek/smoof")
library(smoof)

# NOTE: this is not working yet!It is just an exmaple of the planned "workflow"

# EXAMPLES: HOMOGENEOUS NK-LANDSCAPES
# ===

# Parameters
M = 2L
N = 50L
K = 3L

set.seed(123)

# examplary bit-string
x = sample(c(0, 1), size = N, replace = TRUE)

# generate two single-objective NK-landscapes
fn1 = makeNKFunction(N, K)
fn2 = makeNKFunction(N, K)

# export and import
path = "instances/my_NK_function"
export(fn1, path)
fn1 = import(path)

# build a two-objective MNK-landscape
moofn = makeMNKFunction(M, N, K)

# build "heterogenous" MNK-landscape (different k for every objective)
moofn = makeMNKFunction(M, N, K = c(2L, 5L))

# alternative constructor by passing two single-objective NK-landscapes
moofn = makeMNKFunction(funs = c(fn1, fn2))

# another alternative passing path names to exported single-objective NK-landscapes
moofn = makeMNKFunction(funs = c("instances/fn1", "instances/fn2"))

# call :)
moofn(x)

# EXAMPLES: HETEROGENEOUS NK-LANDSCAPES
# ===

K_min = 2L
K_max = 5L

# different K for every bit sampled from U(K_min, K_max)
K = sample(K_min:K_max), size = N, replace = TRUE)

# In addition: use Cauchy distribution (instead of Normal-distribution) to
# sample function values f_i, i = 1, ..., N
fn1 = makeNKFunction(N, K, distr = pcauchy)

