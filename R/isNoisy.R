#' Checks whether the given function is noisy.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
isNoisy = function(fn) {
  assertClass(fn, "smoof_function")
  return(attr(fn, "noisy"))
}
