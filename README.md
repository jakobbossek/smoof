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
|Bartels Conn                              |
|Set of noiseless BOBB Function(s)         |
|Beale                                     |
|Bent-Cigar                                |
|Bird                                      |
|BiSphere                                  |
|BK1                                       |
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
|Deckkers-Aarts                            |
|Deflected Corrugated Spring               |
|Dent                                      |
|Dixon-Price                               |
|Double-Sum                                |
|DTLZ1                                     |
|DTLZ2                                     |
|DTLZ3                                     |
|DTLZ4                                     |
|DTLZ5                                     |
|DTLZ6                                     |
|DTLZ7                                     |
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
|GOMOP                                     |
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
|Matyas                                    |
|McCormick                                 |
|Michalewicz                               |
|MOP1                                      |
|MOP2                                      |
|MOP3                                      |
|MOP4                                      |
|MOP5                                      |
|MOP6                                      |
|MOP7                                      |
|Multiple peals model 2 function generator |
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
|Swiler2014                                |
|Three-Hump Camel                          |
|Trecanni                                  |
|UF1, ..., UF10 of the CEC 2009            |
|Viennet                                   |
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

smoof v1.5 (Release data: soon)
============

* Fixed: mean function was not set properly for noisy functions
* Improved docs slighly
* Added shortcuts {s,m}nof for generating single-/multi-objective objective functions
  with numeric parameters only
* Fixed: precision issue caused by rPython respectively RJSONIO::toJSON, which by
  default strips numeric values to only 4 digits.
* Fixed: ggplot2 warnings
* Fixed: wrong lower bound in MOP3 test function
* Fixed: issue in definition of Cosine Mixture Function due to faulty online sources
* Fixed: issue with min<->max conversion if functions has.simple.signature = FALSE and
  explicit return statement was used.
* Added: missing single-objective tag for BBOB functions
* Added: log.scale argument for autoplot
* Added: CITATION file

smoof v1.4 (Release data: 2016-08-03)
============

* Fixed: issue in formula and global optimum of BukinN2 function
* overworked and refactored autoplot functions
  * dropped use.facets parameter (always use facets now if discrete parameters exist)
  * We now support mixed functions with up to two numeric params (or one numeric vector
    param of length 2) and up to 2 discrete/logical (or a corresponding vector param)
* Added makeGOMOPFunction to create multi-objective test function based on a set of
  single objective functions.
* Added new single-objective functions: Branin (modified version by Forrester et al. (2008))
* Added new multi-objective functions: Van Valedhuizen's test suite (MOP1-7), Binh-Korn function,
  BiSphere (bi-objective Sphere), Dent function, Viennet function.
* Added first mixed parameter space funtion: Zhou2011
* visualizeParetoOptimalFront now draws lines instead of points
* Added possibility to draw interactive 3D surface plots via smoof::plot3D(fn, package = "plotly")
* Removed S3 method definition of getParamSet. This function is now contained in ParamHelpers 1.8

smoof v1.3 (Release data: 2016-02-23)
============

* Modified: function name is optional now
* Added optional reference point ref.point for multi-objective functions
  - Reference point for ZDT functions is (11, 11)
  - Reference point for DTLZ function family is r = (11, ...,  11) with #r = #objectives
  - Added getter getRefPoint
* Added possibility to pass the true mean function of a noisy function, i.e., the
  "unnoisy" via the smoof parameter fn.mean
  - Added getter getMeanFunction
* makeMPM2Function now has additional parameters rotated and peak.shape

smoof v1.2 (Release data: 2016-01-21)
============

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

smoof v1.1 (Release date: 2015-11-24):
============

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

smoof v1.0 (Release date: 2015-05-19):
==========

* First submission to CRAN.

## Citation

Please cite my [R Journal paper](https://journal.r-project.org/archive/2017/RJ-2017-004/index.html) in publications. Get the information via `citation("smoof")` or use the following BibTex entry:
```
@Article{,
  author = {Jakob Bossek},
  title = {smoof: Single- and Multi-Objective Optimization Test Functions},
  year = {2017},
  journal = {The R Journal},
  url = {https://journal.r-project.org/archive/2017/RJ-2017-004/index.html},
}
```

## Contact

Please address questions and missing features about the **smoof package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/smoof/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem.
