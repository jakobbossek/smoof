#' Extract logged values of a function wrapped by a logging wrapper.
#'
#' @param fn [\code{smoog_function}]\cr
#'   Smoof function.
#' @return [\code{list}]
#'   List containing the following values
#'   \describe{
#'     \item{pars}{Data frame of parameter values, i.e., x-values or the empty
#'     data frame if x-values were not logged.}
#'     \item{obj.vals}{Numeric vector of objective vals in the single-objective
#'     case respectively a matrix of objective vals for multi-objective functions.}
#'   }
getLoggedValues = function(fn) {
  UseMethod("getLoggedValues")
}

#' @export
getLoggedValues.smoof_logging_function = function(fn) {
  env = environment(fn)
  pars = env$pars
  obj.vals = env$obj.vals
  if (nrow(obj.vals) == 1L) {
    obj.vals = as.numeric(obj.vals)
  }
  return(list(pars = pars, obj.vals = obj.vals))
}

#' @export
getLoggedValues.smoof_wrapped_function = function(fn) {
  return(getLoggedValues(getWrappedFunction(fn)))
}
