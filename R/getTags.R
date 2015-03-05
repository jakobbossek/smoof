#' Returns vector of associated tags.
#'
#' @template arg_smoof_function
#' @return [\code{character}]
#' @export
getTags = function(fn) {
  assertClass(fn, "smoof_function")
  attr(fn, "tags")
}
