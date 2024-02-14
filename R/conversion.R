#' @title
#' Conversion between minimization and maximization problems.
#'
#' @description
#' We can minimize f by maximizing -f. The majority of predefined objective functions
#' in \pkg{smoof} should be minimized by default. However, there are a handful of
#' functions, e.g., Keane or Alpine02, which shall be maximized by default.
#' For benchmarking studies it might be beneficial to inverse the direction.
#' The functions \code{convertToMaximization} and \code{convertToMinimization}
#' do exactly that, keeping the attributes.
#'
#' @note
#' Both functions will quit with an error if multi-objective functions are passed.
#'
#' @param fn [\code{smoof_function}]\cr
#'  Smoof function.
#' @return [\code{smoof_function}]
#'  Converted smoof function
#' @examples
#' # create a function which should be minimized by default
#' fn = makeSphereFunction(1L)
#' print(shouldBeMinimized(fn))
#' # Now invert the objective direction ...
#' fn2 = convertToMaximization(fn)
#' # and invert it again
#' fn3 = convertToMinimization(fn2)
#' # Now to convince ourselves we render some plots
#' opar = par(mfrow = c(1, 3))
#' plot(fn)
#' plot(fn2)
#' plot(fn3)
#' par(opar)
#' @name conversion
#' @rdname conversion
#' @export
convertToMaximization = function(fn) {
  convertProblemDirection(fn, minimize.after = FALSE)
}

#' @rdname conversion
#' @export
convertToMinimization = function(fn) {
  convertProblemDirection(fn, minimize.after = TRUE)
}

convertProblemDirection = function(fn, minimize.after = TRUE) {
  checkmate::assertFlag(minimize.after)

  if (isWrappedSmoofFunction(fn)) {
    BBmisc::stopf("Conversion works only for unwrapped functions! Apply, e.g., counting wrapper
      after conversion.")
  }

  if (isMultiobjective(fn)) {
    BBmisc::stopf("Conversion to maximization only supported for single-objective problems
      at the moment, but your function '%s' has %i objectives.", getName(fn), getNumberOfObjectives(fn))
  }

  # If both are true, we want to convert min to min
  # If both are false, we want to convert max to max
  # Otherwise the conversion is ok
  if ((shouldBeMinimized(fn) && minimize.after) || (!shouldBeMinimized(fn) && !minimize.after)) {
    BBmisc::stopf("Function should already be %s.", (if (minimize.after) "minimized" else "maximized"))
  }

  # get attributes
  attribs = attributes(fn)
  should.minimize = shouldBeMinimized(fn)
  attributes(fn) = NULL

  # make wrapper function
  fn2 = function(x) {
    -fn(x)
  }

  # copy attributes (get dropped on body() call)
  attributes(fn2) = attribs

  # flip sign(s) of optima
  if (hasGlobalOptimum(fn2))
    fn2 = BBmisc::setAttribute(fn2, "global.opt.value", -1.0 * attr(fn2, "global.opt.value"))

  if (hasLocalOptimum(fn2))
    fn2 = BBmisc::setAttribute(fn2, "local.opt.value", -1.0 * attr(fn2, "local.opt.value"))

  # flip maximization stuff
  fn2 = BBmisc::setAttribute(fn2, "minimize", !should.minimize)
  return(fn2)
}
