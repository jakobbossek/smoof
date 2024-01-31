#' Checks whether the global optimum is known.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#'  \code{TRUE} if the global optimum is known, \code{FALSE} otherwise.
#' @export
hasGlobalOptimum = function(fn) {
  UseMethod("hasGlobalOptimum")
}

#' @export
hasGlobalOptimum.smoof_single_objective_function = function(fn) {
  return(!is.null(attr(fn, "global.opt.params")))
}

#' @export
hasGlobalOptimum.smoof_multi_objective_function = function(fn) {
  return(FALSE)
}

#' @export
hasGlobalOptimum.smoof_wrapped_function = function(fn) {
  return(hasGlobalOptimum(getWrappedFunction(fn)))
}

#' Checks whether local optima are known.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
hasLocalOptimum = function(fn) {
  UseMethod("hasLocalOptimum")
}

#' @export
hasLocalOptimum.smoof_single_objective_function = function(fn) {
  return(!is.null(attr(fn, "local.opt.params")))
}

#' @export
hasLocalOptimum.smoof_multi_objective_function = function(fn) {
  return(FALSE)
}

#' @export
hasLocalOptimum.smoof_wrapped_function = function(fn) {
  return(hasLocalOptimum(getWrappedFunction(fn)))
}
