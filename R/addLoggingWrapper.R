#' @title
#' Return a function which internally stores x or y values.
#'
#' @description
#' Often it is desired and useful to store the optimization path, i.e., the evaluated
#' function values and/or the parameters. Not all optimization algorithms offer
#' such a trace. This wrapper makes a smoof function handle x/y-values itself.
#'
#' @param fn [\code{smoof_function}]\cr
#'   Smoof function.
#' @param logg.x [\code{logical(1)}]\cr
#'   Should x-values be logged?
#'   Default is \code{FALSE}.
#' @param logg.y [\code{logical(1)}]\cr
#'   Should objective values be logged?
#'   Default is \code{TRUE}.
#' @param size [\code{integer(1)}]\cr
#'   Initial size of the internal data structures used for logging.
#'   Default is 100. I.e., there is space reserved for 100 function evaluations.
#'   In case of an overflow (i.e., more function evaluations than space reserved)
#'   the data structures are re-initialized by adding space for another \code{size} evaluations.
#'   This comes handy if you know the number of function evaluations (or at least
#'   an upper bound thereof) a-priori and may serve to reduce the time complextity
#'   of logging values.
#' @return [\code{smoof_logging_function}]
#' @examples
#' # We first build the smoof function and apply the logging wrapper to it
#' fn = makeSphereFunction(dimensions = 2L)
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
addLoggingWrapper = function(fn, logg.x = FALSE, logg.y = TRUE, size = 100L) {
  if (!testClass(fn, "smoof_function") && !testClass(fn, "smoof_wrapped_function")) {
    stopf("The passed function needs to be a (wrapped) smoof function.")
  }
  assertFlag(logg.x)
  assertFlag(logg.y)

  size = checkmate::asInt(size, lower = 1L)

  if (!logg.x && !logg.y) {
    stopf("At least x or y values must be logged.")
  }

  par.set = ParamHelpers::getParamSet(fn)
  par.ids = getParamIds(par.set, with.nr = TRUE, repeated = TRUE)
  n.obj = getNumberOfObjectives(fn)
  n.pars = getNumberOfParameters(fn)

  curr.idx = 1L
  max.idx = size
  force(curr.idx)
  force(max.idx)
  force(fn)
  force(logg.x)
  force(logg.y)
  force(size)
  force(n.pars)
  force(n.obj)

  # since we need to consider both single and multi-objective functions,
  # we store everything in a (n.obj x evals) matrix.
  obj.vals = pars = NULL
  if (logg.x) {
    pars = data.frame(matrix(NA, nrow = size, ncol = n.pars), stringsAsFactors = FALSE)
    #pars = data.frame(stringsAsFactors = FALSE)
  }
  if (logg.y) {
    obj.vals = matrix(NA, nrow = n.obj, ncol = size)
  }


  wrapped.fn = function(x, ...) {
    # re-scale
    if (curr.idx > max.idx) {
      # extend data structures by another 'size' rows/columns
      pars <<- rbind(pars, data.frame(matrix(NA, nrow = size, ncol = n.pars), stringsAsFactors = FALSE))
      obj.vals <<- cbind(obj.vals, matrix(NA, nrow = n.obj, ncol = size))
      max.idx <<- max.idx + size
    }

    # convert everything to a list
    if (is.matrix(x)) {
      x = apply(x, 2L, function(el) {
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
        obj.vals[, curr.idx] <<- y.curr
        #obj.vals <<- cbind(obj.vals, y.curr, deparse.level = 0)
      }
      if (logg.x) {
        pars[curr.idx, ] <<- par
        #pars <<- rbind(pars, as.data.frame(par))
      }
      curr.idx <<- curr.idx + 1L
      return(y.curr)
    })
    return(y)
  }
  class(wrapped.fn) = c("smoof_logging_function", "smoof_wrapped_function", "smoof_function", "function")
  return(wrapped.fn)
}


