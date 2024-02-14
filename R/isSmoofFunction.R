#' Checks whether the given object is a \code{smoof_function} or a
#' \code{smoof_wrapped_function}.
#'
#' @param object [any]\cr
#'   Arbitrary R object.
#' @return [\code{logical(1)}]
#'  \code{TRUE} if the object is a SMOOF function or wrapped function, \code{FALSE} otherwise.
#' @seealso \code{\link{addCountingWrapper}}, \code{\link{addLoggingWrapper}}
#' @export
isSmoofFunction = function(object) {
  return(inherits(object, c("smoof_function", "smoof_wrapped_function")))
}
