#' Checks whether the given function accept \dQuote{vectorized} input.
#'
#' @template arg_smoof_function
#' @return [\code{logical(1)}]
#' @export
isVectorized = function(fn) {
  assertClass(fn, "smoof_function")
  return(attr(fn, "vectorized"))
}
