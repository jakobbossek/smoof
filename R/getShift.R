#' Return the outer most shift.
#'
#' @template arg_smoof_function
#' @param all [\code{logical(1)}]\cr
#'   Return the sum of all shifts if \code{TRUE}, else return only the outer most shift.
#'   Default \code{FALSE}.
#' @return [\code{numeric}]
#' @export
getShift = function(fn, all=FALSE) {
  UseMethod("getShift")
}

#' @export
getShift.smoof_function = function(fn, all=FALSE) {
  rep_len(0, getNumberOfParameters(fn))
}

#' @export
getShift.smoof_wrapped_function = function(fn, all=FALSE) {
  getShift(getWrappedFunction(fn), all)
}

#' @export
getShift.smoof_shifted_function = function(fn, all=FALSE) {
  shift <- environment(fn)$shift
  if (all)
    shift <- shift + getShift(getWrappedFunction(fn), all)
  shift
}

