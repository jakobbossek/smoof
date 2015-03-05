#' Return description of the function.
#'
#' @template arg_smoof_function
#' @return [\code{character(1)}]
#' @export
getDescription = function(fn) {
  assertClass(fn, "smoof_function")
  return(attr(fn, "description"))
}
