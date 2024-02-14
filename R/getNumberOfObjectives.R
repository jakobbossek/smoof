#' Determines the number of objectives.
#'
#' @template arg_smoof_function
#' @return [\code{integer(1)}]
#'  The number of objectives.
#' @export
getNumberOfObjectives = function(fn) {
  UseMethod("getNumberOfObjectives")
}

#' @export
getNumberOfObjectives.smoof_function = function(fn) {
  return(attr(fn, "n.objectives"))
}

#' @export
getNumberOfObjectives.smoof_wrapped_function = function(fn) {
  return(getNumberOfObjectives(getWrappedFunction(fn)))
}
