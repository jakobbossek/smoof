# otf: Optimization target functions

Travis CI build status: [![Build Status](https://travis-ci.org/jakobbossek/otf.svg)](https://travis-ci.org/jakobbossek/otf)

This package offers an interface for target functions in the context of function optimization. It conveniently builds up on the S3 objects, i. e., a target function is a S3 object composed of a descriptive name, the function itself, a parameter set, eventually box constraints or other constraints, number of targets and so on. Wrapping your target functions inside these S3 methods seems to bloat the code, and in fact it does in some way, but it is reasonable and using it makes things more definitely more readable.

## Installation instructions

The package is currently under developmenent and has not yet been released on CRAN. If you want to take a glance at it and play around install the github developement version by executing the following command:

```splus
devtools::install_github("otf", username = "jakobbossek")
```

## Example

Let us consider the problem of finding the (global) minimum of the multimodal target function f(x) = x sin(3x) on the closed intervall [0, 2PI]. We define our target function via the ```makeSingleObjectiveFunction()``` method providing a name, the function itself and a parameter set. We can display the function within the box constraints with ggplot and search for the global optimum with the CMA Evolutionary Strategy as follows:

```splus
library(ggplot2)
library(cmaes)

obj.fn = makeSingleObjectiveFunction(
	name = "My fancy function name",
	fn = function(x) x * sin(3*x),
	par.set = makeParamSet(
		makeNumericParam("x", lower = 0, upper = 2*pi)
	)
)

autoplot(obj.fn)

set.seed(123)
res = cma_es(
	fn = obj.fn,
	lower = 0,
	upper = 2 * pi,
	par = 3,
	control = list(lambda = 100)
)
print(res)
```

Packages like [ecr](https://github.com/jakobbossek/ecr), [mlrMBO](https://github.com/berndbischl/mlrMBO) and [noir](https://github.com/jakobbossek/noir) require the passed objective function to be of otf type.

## Contact

Please address questions and missing features about the **otf package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jbossek/otf/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.
