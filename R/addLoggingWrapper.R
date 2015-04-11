#' Return a function which internally stores x or y values.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function.
#' @param logg.x [\code{logical(1)}]\cr
#'   Should x-values be logged? Default is \code{FALSE}.
#' @param logg.y [\code{logical(1)}]\cr
#'   Should objective values be logged? Default is \code{TRUE}.
#' @return [\code{smoof_logging_function}]
#' @examples
#' fn = makeSphereFunction(dimension = 2L)
#' fn = addLoggingWrapper(fn, logg.x = TRUE)
#' res = optim(fn, par = c(1, 1), method = "Nelder-Mead")
#' log.res = getLoggedValues(fn)
#' print(fn$pars)
#' print(fn$obj.vals)
#'
#' @note Logging values, in particular logging x-values, will substantially slow
#' down the evaluation of the function.
#'
#' @export
addLoggingWrapper = function(fn, logg.x = FALSE, logg.y = TRUE) {
  assertClass(fn, "smoof_function")
  assertFlag(logg.x, na.ok = FALSE)
  assertFlag(logg.y, na.ok = FALSE)

  if (!logg.x && !logg.y) {
    stopf("At least x or y values must be logged.")
  }

  force(fn)
  par.set = smoof::getParamSet(fn)
  par.ids = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  n.obj = getNumberOfObjectives(fn)
  n.pars = getNumberOfParameters(fn)

  # since we need to consider both single and multi-objective functions,
  # we store everything in a (n.obj x evals) matrix.
  obj.vals = matrix(0, nrow = n.obj, ncol = 0L)
  pars = data.frame(stringsAsFactors = FALSE)

  wrapped.fn = function(x, ...) {
    #FIXME: handle matrix input
    # call wrapped function
    y = fn(x, ...)
    if (logg.y) {
      obj.vals <<- cbind(obj.vals, y)
    }
    if (logg.x) {
      if (!is.list(x)) {
        x = as.list(x)
        names(x) = par.ids
      }
      pars <<- rbind(pars, as.data.frame(x))
    }
    return(y)
  }
  class(wrapped.fn) = c("smoof_logging_function", "smoof_wrapped_function")
  return(wrapped.fn)
}
