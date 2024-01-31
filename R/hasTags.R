#' @title
#' Checks if the function has assigned special tags.
#'
#' @description
#' Each single-objective smoof function has tags assigned to it (see
#' \code{\link{getAvailableTags}}). This little helper returns a vector of
#' assigned tags from a smoof function.
#'
#' @param fn [\code{smoof_function}]
#'   Function of \code{smoof_function}, a \code{smoof_generator} or a string.
#' @param tags [\code{character}]\cr
#'   Vector of tags/properties to check \code{fn} for.
#' @return [\code{logical(1)}]
#'  Logical vector indicating the presence of specified tags.
#' @export
hasTags = function(fn, tags) {
  UseMethod("hasTags")
}

#' @export
hasTags.smoof_function = function(fn, tags) {
  checkmate::assertChoice(tags, choices = getAvailableTags())
  return(BBmisc::isSubset(tags, getTags(fn)))
}

#' @export
hasTags.smoof_generator = function(fn, tags) {
  checkmate::assertChoice(tags, choices = getAvailableTags())
  return(BBmisc::isSubset(tags, attr(fn, "tags")))
}

#' @export
hasTags.character = function(fn, tags) {
  checkmate::assertChoice(tags, choices = getAvailableTags())
  generator = getGeneratorByName(fn)
  if (is.null(generator)) {
    BBmisc::stopf("No generator for function '%s'", fn)
  }
  return(hasTags(generator, tags))
}

hasTags.smoof_wrapped_function = function(fn, tags) {
  return(hasTags(getWrappedFunction(fn), tags))
}
