#' Extract logged values of a function wrapped by a logging wrapper.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function.
#' @param compact [\code{logical(1)}]\cr
#'   Wrap all logged values in a single data frame? Default is \code{FALSE}.
#' @return [\code{list} || \code{data.frame}]
#'   If \code{compact} is \code{TRUE}, a single data frame. Otherwise the function
#'   returns a list containing the following values.
#'   \describe{
#'     \item{pars}{Data frame of parameter values, i.e., x-values or the empty
#'     data frame if x-values were not logged.}
#'     \item{obj.vals}{Numeric vector of objective vals in the single-objective
#'     case respectively a matrix of objective vals for multi-objective functions.}
#'   }
#' @export
getLoggedValues = function(fn, compact = FALSE) {
  UseMethod("getLoggedValues")
}

#' @export
getLoggedValues.smoof_logging_function = function(fn, compact = FALSE) {
  env = environment(fn)
  pars = env$pars
  obj.vals = env$obj.vals
  # wrap everything up in a single data frame
  if (compact) {
    obj.vals = as.data.frame(t(obj.vals))
    names(obj.vals) = paste0("y", ncol(obj.vals))
    return(cbind(pars, obj.vals))
  }
  if (nrow(obj.vals) == 1L) {
    obj.vals = as.numeric(obj.vals)
  }
  return(list(pars = pars, obj.vals = obj.vals))
}

#' @export
getLoggedValues.smoof_wrapped_function = function(fn, compact = FALSE) {
  return(getLoggedValues(getWrappedFunction(fn), compact))
}