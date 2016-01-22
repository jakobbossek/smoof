# smoof: Single- and Multi-Objective Optimization test Functions

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/smoof)](http://cran.r-project.org/web/packages/smoof)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/smoof)](http://cran.rstudio.com/web/packages/smoof/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/smoof.svg)](https://travis-ci.org/jakobbossek/smoof)
[![Build status](https://ci.appveyor.com/api/projects/status/4b468f5phkb4lmeq/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/smoof/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/smoof/badge.svg)](https://coveralls.io/r/jakobbossek/smoof)

This package offers an interface for objective functions in the context of (multi-objective) global optimization. It conveniently builds up on the S3 objects, i. e., an objective function is a S3 object composed of a descriptive name, the function itself, a parameter set, box constraints or other constraints, number of objectives and so on. Moreover, the package contains generators for a load of both single- and multi-objective optimization test functions which are frequently being used in the literature of (benchmarking) optimization algorithms.
The bi-objective ZDT function family by Zitzler, Deb and Thiele is included as well as the popular single-objective test functions like De Jong's function, Himmelblau function and Schwefel function. Moreover, the package offers a R interface to the C implementation of the *Black-Box Optimization Benchmarking* (BBOB) [set of noiseless test functions](http://coco.gforge.inria.fr/doku.php?id=bbob-2009-downloads).

![examplary smoof functions](https://raw.githubusercontent.com/jakobbossek/smoof/screenshots/smoof_funs_example.png)

At the moment the following optimization test functions respectively function sets/families are implemented:


|Function                                  |
|:-----------------------------------------|
|Ackley                                    |
|Adjiman                                   |
|Alpine N. 1                               |
|Alpine N. 2                               |
|Aluffi-Pentini                            |
|Set of noiseless BOBB Function(s)         |
|Bartels Conn                              |
|Beale                                     |
|Bent-Cigar                                |
|Bird                                      |
|Bohachevsky N. 1                          |
|Booth                                     |
|BraninRCOS                                |
|Brent                                     |
|Brown                                     |
|Bukin N. 2                                |
|Bukin N. 4                                |
|Bukin N. 6                                |
|Carrom Table                              |
|Chichinadze                               |
|Chung Reynolds                            |
|Complex                                   |
|Cosine Mixture                            |
|Cross-In-Tray                             |
|Cube                                      |
|DTLZ1                                     |
|DTLZ2                                     |
|DTLZ3                                     |
|DTLZ4                                     |
|DTLZ5                                     |
|DTLZ6                                     |
|DTLZ7                                     |
|Deckkers-Aarts                            |
|Deflected Corrugated Spring               |
|Dixon-Price                               |
|Double-Sum                                |
|Eason                                     |
|Egg Crate                                 |
|Egg Holder                                |
|El-Attar-Vidyasagar-Dutta                 |
|Engvall                                   |
|Exponential                               |
|Freudenstein Roth                         |
|Generelized Drop-Wave                     |
|Giunta                                    |
|Goldstein-Price                           |
|Griewank                                  |
|Hansen                                    |
|Himmelblau                                |
|Holder Table N. 1                         |
|Holder Table N. 2                         |
|Hosaki                                    |
|Hyper-Ellipsoid                           |
|Jennrich-Sampson                          |
|Judge                                     |
|Keane                                     |
|Kearfott                                  |
|Leon                                      |
|Multiple peals model 2 function generator |
|Matyas                                    |
|McCormick                                 |
|Michalewicz                               |
|Periodic                                  |
|Double-Sum                                |
|Price N. 1                                |
|Price N. 2                                |
|Price Function N. 4                       |
|Rastrigin                                 |
|Rosenbrock                                |
|Schaffer N. 2                             |
|Schaffer N. 4                             |
|Schwefel                                  |
|Shubert                                   |
|Six-Hump Camel Back                       |
|Sphere                                    |
|Styblinkski-Tang                          |
|Sum of Different Squares                  |
|Three-Hump Camel                          |
|Trecanni                                  |
|UF1, ..., UF10 of the CEC 2009            |
|ZDT1                                      |
|ZDT2                                      |
|ZDT3                                      |
|ZDT4                                      |
|ZDT6                                      |
|Zettl                                     |

## Installation instructions

Visit the [package repository on CRAN](http://cran.r-project.org/web/packages/smoof/index.html). If you want to take a glance at the developement version install the github developement version by executing the following command:

```splus
devtools::install_github("jakobbossek/smoof")
```

## Example

### Use a build-in generator
Assume the simplifying case where we want to benchmark a set of optimization algorithms on a single objective instance. We decide ourselves for the popular 10-dimensional Rosenbrock banana function. Instead of looking up the function definition, the box constraints and where the global optimum is located, we simply generate the function with **smoof** and get all the stuff:

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
  par.set = makeNumericParamSet("x", len = 1L, lower = 0, upper = 2 * pi)
)
print(obj.fn)
print(getParamSet(obj.fn))
print(autoplot(obj.fn))
```

The [ecr](https://github.com/jakobbossek/ecr) package for evolutionary computing in R needs builds upon smoof functions.

## News

### smoof v1.2 (Release data: 2016-01-21)

* Added: functions convertToMaximization and convertToMinimization
* Added: main parameter for plot and autoplot. By default the function name is
  used for the plot title.
* Fixed: autoplot and plot do not work for wrapped functions.
* objective functions now can be passed an additional id attribute. All predefined
  smoof functions have an id now.
* visualizeParetoOptimalFront now works for bi-objective functions with arbitrary
  search space dimensions and works by calling the mco::nsga2 algorithm. Due to
  this, the parameters show.only.front, limits.by.front are dropped.
* Renamed makeFunctionByName to makeFunctionsByName
* Added tags 'single-objective' and 'multi-objective'
* filterFunctionByTags now stops if both 'single-objective' and 'multi-objective'
  tags are passed
* All functions now check the passed paramter to be of the right dimension and
  type
* makeFunctionsByName now expects a character vector generator names (the list
  methods was removed)
* Generator names now do not contain the 'Function' suffix
* Fixed: hasConstraints for wrapped smoof functions
* Fixed: getUpperBoxConstraints

### smoof v1.1 (Release date: 2015-11-24):

* Parameter set of predefined smoof function now contains a single vector parameter
  instead of multiple single numeric parameters. This is consistent with function
  calls now, since these always expect a single vector or list.
* Added helper function get{Lower,Upper}BoxConstraints
* smoof functions now expect an optional 'minimize' argument which indicates which
  objectives should be minimized or maximized respectively
* Fixed some wrong tag assigments
* Added shouldBeMinimized function
* Fixed global optimum of Giunta function
* Added function makeFunctionByName, which expects a function name or a list of
  functions names. The corresponding generator(s) is/are  called. Useful if you want,
  e.g., filter functions by tags and generate them directly afterwards.
* Added hasTags helper function.
* filterFunctionByTags now has an additional logical argument 'or'. If this is set
  to TRUE, a subset of the passed tags is sufficient to select a function.
* Added multi-objective DTLZ function family
* Added 2D single objective functions: Aluffi-Pentini-(Zirilli), Complex, Engvall,
  Jennrich-Sampsam, Judge, Kearfott
* Renamed bochachevsky function to bohachevsky.n1

###smoof v1.0 (Release date: 2015-05-19):

* First submission to CRAN.

## Contact

Please address questions and missing features about the **smoof package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/smoof/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.
