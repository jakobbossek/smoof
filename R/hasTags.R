#' @title Check if function has assigend special tags.
#'
#' @param fn [\code{smoof_function}]
#'   Function of \code{smoof_function}, a \code{smoof_generator} or a string.
#' @param tags [\code{character}]\cr
#'   Vector of tags/properties to check \code{fn} for.
#' @return [\code{logical(1)}]
#' @export
hasTags = function(fn, tags) {
  UseMethod("hasTags")
}

#' @export
hasTags.smoof_function = function(fn, tags) {
  assertChoice(tags, choices = getAvailableTags())
  return(isSubset(tags, getTags(fn)))
}

#' @export
hasTags.smoof_generator = function(fn, tags) {
  assertChoice(tags, choices = getAvailableTags())
  return(isSubset(tags, attr(fn, "tags")))
}

#' @export
hasTags.character = function(fn, tags) {
  assertChoice(tags, choices = getAvailableTags())
  generator = getGeneratorByName(fn)
  if (is.null(generator)) {
    stopf("No generator for function '%s'", fn)
  }
  return(hasTags(generator, tags))
}

hasTags.smoof_wrapped_function = function(fn, tags) {
  return(hasTags(getWrappedFunction(fn), tags))
}
