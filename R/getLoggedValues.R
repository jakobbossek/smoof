#' Extracts the logged values of a function wrapped by a logging wrapper.
#'
#' @param fn [\code{wrapped_smoof_function}]\cr
#'   Wrapped smoof function.
#' @param compact [\code{logical(1)}]\cr
#'   Wrap all logged values in a single data frame? Default is \code{FALSE}.
#' @return [\code{list} || \code{data.frame}]
#'   If \code{compact} is \code{TRUE}, a single data frame. Otherwise the function
#'   returns a list containing the following values:
#'   \describe{
#'     \item{pars}{Data frame of parameter values, i.e., x-values or the empty
#'     data frame if x-values were not logged.}
#'     \item{obj.vals}{Numeric vector of objective values in the single-objective
#'     case respectively a matrix of objective values for multi-objective functions.}
#'   }
#' @seealso \code{\link{addLoggingWrapper}}
#' @export
getLoggedValues = function(fn, compact = FALSE) {
  UseMethod("getLoggedValues")
}

#' @export
getLoggedValues.smoof_logging_function = function(fn, compact = FALSE) {
  checkmate::assertFlag(compact)

  env = environment(fn)
  max.idx = env$curr.idx - 1L
  pars = NULL
  if (env$logg.x) {
    pars = env$pars[seq_len(max.idx), , drop = FALSE]
    colnames(pars) = getParamIds(ParamHelpers::getParamSet(fn), with.nr = TRUE, repeated = TRUE)
  }

  obj.vals = NULL
  if (env$logg.y)
    obj.vals = env$obj.vals[, seq_len(max.idx), drop = FALSE]
  # wrap everything up in a single data frame
  if (compact) {
    # if only the x-values are stored just return the data frame
    if (!env$logg.y) {
      return(pars)
    }
    df = as.data.frame(t(obj.vals))
    names(df) = paste0("y", seq(ncol(df)))
    if (env$logg.x) {
      # append x-values if stored
      df = cbind(pars, df)
    }
    return(df)
  }
  if (!is.null(obj.vals) && nrow(obj.vals) == 1L) {
    obj.vals = as.numeric(obj.vals)
  }
  return(list(pars = pars, obj.vals = obj.vals))
}

#' @export
getLoggedValues.smoof_wrapped_function = function(fn, compact = FALSE) {
  return(getLoggedValues(getWrappedFunction(fn), compact))
}
