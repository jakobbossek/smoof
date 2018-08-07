# smoof 1.6

## New features

* Added bi-objective BBOB problem generator: makeBiObjBBOBFunction
* Added: Kursawe function
* Added: ED1 and ED2 functions

# smoof 1.5.1

## New features

* Added: Shekel function
* Added: inverted Vincent function
* Added: modified Rastrigin function
* Added: Hartmann{3,4,6} functions

## Bugfixes

* Better handling of additional arguments in makeFunctionsByName
* convertTo{Minimization,Maximization} now flips sign of optima

## Miscellaneous

* logging and couting wrapper now have class smoof_function

# smoof 1.5

## New features

* Added shortcuts {s,m}nof for generating single-/multi-objective objective functionswith numeric parameters only
* Added: missing single-objective tag for BBOB functions
* Added: log.scale argument for autoplot
* Added: CITATION file

## Bugfixes

* Fixed: precision issue caused by rPython respectively RJSONIO::toJSON, which by
  default strips numeric values to only 4 digits.
* Fixed: ggplot2 warnings
* Fixed: wrong lower bound in MOP3 test function
* Fixed: issue in definition of Cosine Mixture Function due to faulty online sources
* Fixed: issue with min<->max conversion if functions has.simple.signature = FALSE and explicit return statement was used.
* Fixed: mean function was not set properly for noisy functions
* Improved docs slighly

# smoof 1.4

## New features

* Added makeGOMOPFunction to create multi-objective test function based on a set of
  single objective functions.
* Added new single-objective functions: Branin (modified version by Forrester et al. (2008))
* Added new multi-objective functions: Van Valedhuizen's test suite (MOP1-7), Binh-Korn function,
  BiSphere (bi-objective Sphere), Dent function, Viennet function.
* Added first mixed parameter space funtion: Zhou2011
* visualizeParetoOptimalFront now draws lines instead of points
* Added possibility to draw interactive 3D surface plots via smoof::plot3D(fn, package = "plotly")

## Bugfixes

* Fixed: issue in formula and global optimum of BukinN2 function
* overworked and refactored autoplot functions
  * dropped use.facets parameter (always use facets now if discrete parameters exist)
  * We now support mixed functions with up to two numeric params (or one numeric vector
    param of length 2) and up to 2 discrete/logical (or a corresponding vector param)
* visualizeParetoOptimalFront now draws lines instead of points
* Removed S3 method definition of getParamSet. This function is now contained in ParamHelpers 1.8

# smoof 1.3

## New features

* Added optional reference point ref.point for multi-objective functions
  - Reference point for ZDT functions is (11, 11)
  - Reference point for DTLZ function family is r = (11, ...,  11) with #r = #objectives
  - Added getter getRefPoint
* Added possibility to pass the true mean function of a noisy function, i.e., the
  "unnoisy" via the smoof parameter fn.mean
  - Added getter getMeanFunction
* makeMPM2Function now has additional parameters rotated and peak.shape
* Modified: function name is optional now

# smoof 1.2

## New features

* Added: functions convertToMaximization and convertToMinimization
* Added: main parameter for plot and autoplot. By default the function name is
  used for the plot title.
* objective functions now can be passed an additional id attribute. All predefined smoof functions have an id now.
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

## Bugfixes

* Fixed: autoplot and plot do not work for wrapped functions.
* Fixed: hasConstraints for wrapped smoof functions
* Fixed: getUpperBoxConstraints

# smoof 1.1

## New features

* Parameter set of predefined smoof function now contains a single vector parameter
  instead of multiple single numeric parameters. This is consistent with function
  calls now, since these always expect a single vector or list.
* Added helper function get{Lower,Upper}BoxConstraints
* smoof functions now expect an optional 'minimize' argument which indicates which
  objectives should be minimized or maximized respectively
* Added shouldBeMinimized function
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

## Bugfixes

* Fixed some wrong tag assigments
* Fixed global optimum of Giunta function

# smoof v1.0:

* First submission to CRAN.
