# smoof: Single- and Multi-Objective Optimization test Functions

[![CRAN](https://img.shields.io/badge/cran-1.0-green.svg)](http://cran.r-project.org/web/packages/smoof/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/smoof.svg)](https://travis-ci.org/jakobbossek/smoof)
[![Build status](https://ci.appveyor.com/api/projects/status/4b468f5phkb4lmeq/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/smoof/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/smoof/badge.svg)](https://coveralls.io/r/jakobbossek/smoof)

This package offers an interface for objective functions in the context of (multi-objective) global optimization. It conveniently builds up on the S3 objects, i. e., an objective function is a S3 object composed of a descriptive name, the function itself, a parameter set, box constraints or other constraints, number of objectives and so on. Moreover, the package contains generators for a load of both single- and multi-objective optimization test functions which are frequently being used in the literature of (benchmarking) optimization algorithms.
The bi-objective ZDT function family by Zitzler, Deb and Thiele is included as well as the popular single-objective test functions like De Jong's function, Himmelblau function and Schwefel function. Moreover, the package offers a R interface to the C implementation of the *Black-Box Optimization Benchmarking* (BBOB) [set of noiseless test functions](http://coco.gforge.inria.fr/doku.php?id=bbob-2009-downloads).

![examplary smoof functions](https://raw.githubusercontent.com/jakobbossek/smoof/screenshots/smoof_funs_example.png)

At the moment the following optimization test functions respectively function sets/families are implemented:


|Function                                  |
|:-----------------------------------------|
|Ackley Function                           |
|Adjiman Function                          |
|Alpine01 Function                         |
|Alpine02 Function                         |
|Bartels Conn Function                     |
|Set of noiseless BOBB Function(s)         |
|Beale Function                            |
|Bird Function                             |
|Bochachevsky Function                     |
|Booth Function                            |
|Branin Function                           |
|Brent Function                            |
|Brown Function                            |
|Bukin Function N. 2                       |
|Bukin Function N. 4                       |
|Bukin Function N. 6                       |
|Carrom Table Function                     |
|Chichinadze Function                      |
|Chung Reynolds Function                   |
|Cosine Mixture Function                   |
|Cross-In-Tray Function                    |
|Cube Function                             |
|Deckkers-Aarts Function                   |
|Dixon-Price Function                      |
|Double-Sum Function                       |
|Eason Function                            |
|Egg Crate Function                        |
|Egg Holder Function                       |
|El-Attar-Vidyasagar-Dutta Function        |
|Exponential Function                      |
|Freudenstein Roth Function                |
|Generelized Drop-Wave Function            |
|Giunta Function                           |
|Goldstein-Price Function                  |
|Griewank Function                         |
|Hansen Function                           |
|Himmelblau Function                       |
|Holder Table Function N. 1                |
|Holder Table Function N. 2                |
|Hosaki Function                           |
|Hyper-Ellipsoid Function                  |
|Keane Function                            |
|Leon Function                             |
|Matyas Function                           |
|McCormick Function                        |
|Michalewicz Function                      |
|Multiple peals model 2 function generator |
|Periodic Function                         |
|Double-Sum Function                       |
|Price Function N. 1                       |
|Price Function N. 2                       |
|Price Function N. 4                       |
|Rastrigin Function                        |
|Rosenbrock Function                       |
|Schaffer Function N. 2                    |
|Schaffer Function N. 4                    |
|Schwefel function                         |
|Shubert function                          |
|Six-Hump Camel Back Function              |
|Sphere Function                           |
|Styblinkski-Tang Function                 |
|Sum of Different Squares Function         |
|Three-Hump Camel Function                 |
|Trecanni Function                         |
|UF1, ..., UF10 of the CEC 2009            |
|ZDT1 Function                             |
|ZDT2 Function                             |
|ZDT3 Function                             |
|ZDT4 Function                             |
|ZDT6 Function                             |
|Zettl Function                            |

## Installation instructions

Visit the [package repository on CRAN](http://cran.r-project.org/web/packages/smoof/index.html). If you want to take a glance at the developement version install the github developement version by executing the following command:

```splus
devtools::install_github("jakobbossek/smoof")
```

## Example

### Use a build-in generator
Assume the simplifying case where we want to benchmark a set of optimization algorithms on a single objective instance. We decide outself for the popular 10-dimensional Rosenbrock banana function. Instead of looking up the function defintion, the box constraints and where the global optimum is located, we simply generate the function with **smoof** and get all the stuff:

```splus
library(ggplot2)
library(plot3D)

obj.fn = makeRosenbrockFunction(dimensions = 2L)
print(obj.fn)
print(autoplot(obj.fn))
plot3D(obj.fn, length.out = 50L, contour = TRUE)
```

### Set up an objective function by hand
Let us consider the problem of finding the (global) minimum of the multimodal target function f(x) = x sin(3x) on the closed intervall [0, 2PI]. We define our target function via the ```makeSingleObjectiveFunction()``` method providing a name, the function itself and a parameter set. We can display the function within the box constraints with ggplot.

```splus
library(ggplot2)

obj.fn = makeSingleObjectiveFunction(
    name = "My fancy function name",
    fn = function(x) x * sin(3*x),
    par.set = makeParamSet(
        makeNumericParam("x", lower = 0, upper = 2*pi)
    )
)
print(obj.fun)
print(getParamSet(obj.fun))
print(autoplot(obj.fn))
```

The [ecr](https://github.com/jakobbossek/ecr) package for evolutionary computing in R needs builds upon smoof functions.

## Contact

Please address questions and missing features about the **smoof package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/smoof/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.
