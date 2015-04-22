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
#' # We first build the smoof function and apply the logging wrapper to it
#' fn = makeSphereFunction(dimension = 2L)
#' fn = addLoggingWrapper(fn, logg.x = TRUE)
#'
#' # We now apply an optimization algorithm to it and the logging wrapper keeps
#' # track of the evaluated points.
#' res = optim(fn, par = c(1, 1), method = "Nelder-Mead")
#'
#' # Extract the logged values
#' log.res = getLoggedValues(fn)
#' print(log.res$pars)
#' print(log.res$obj.vals)
#' log.res = getLoggedValues(fn, compact = TRUE)
#' print(log.res)
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
  force(logg.x)
  force(logg.y)
  par.set = smoof::getParamSet(fn)
  par.ids = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  n.obj = getNumberOfObjectives(fn)
  n.pars = getNumberOfParameters(fn)

  # since we need to consider both single and multi-objective functions,
  # we store everything in a (n.obj x evals) matrix.
  obj.vals = matrix(0, nrow = n.obj, ncol = 0L)
  pars = data.frame(stringsAsFactors = FALSE)

  wrapped.fn = function(x, ...) {
    # convert everything to a list
    if (is.matrix(x)) {
      x = apply(x, 2, function(el) {
        el = as.list(el)
        names(el) = par.ids
        return(el)
      })
    } else if (is.numeric(x)) {
      x = as.list(x)
      names(x) = par.ids
      x = list(x)
    } else {
      x = list(x)
    }

    y = sapply(x, function(par) {
      y.curr = fn(par, ...)
      if (logg.y) {
        obj.vals <<- cbind(obj.vals, y.curr)
      }
      if (logg.x) {
        pars <<- rbind(pars, as.data.frame(par))
      }
      return(y.curr)
    })
    return(y)
  }
  class(wrapped.fn) = c("smoof_logging_function", "smoof_wrapped_function")
  return(wrapped.fn)
}
