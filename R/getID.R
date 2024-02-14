#' @title
#' Returns the ID / short name of the function.
#' 
#' @description
#' Returns the ID / short name of the function or \code{NA} if no ID is set.
#'
#' @template arg_smoof_function
#' @return [\code{character(1)}] ID / short name or \code{NA}
#' @export
getID = function(fn) {
  UseMethod("getID")
}

#' @export
getID.smoof_function = function(fn) {
  return(attr(fn, "id"))
}

#' @export
getID.smoof_wrapped_function = function(fn) {
  return(getID(getWrappedFunction(fn)))
}
