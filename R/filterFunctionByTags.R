#' Get a list of implemented test functions with specific tags.
#'
#' @param tags [\code{character}]\cr
#'   Character vector of tags. All available tags can be determined with a call
#'   to \code{\link{getAvailableTags}}.
#' @return [\code{character}]
#'   Named vector of function names with the given tags.
#' @examples
#' # show all functions which are unimodal
#' filterFunctionsByTags("unimodal")
#' # show all functions which are both unimodal and separable
#' filterFunctionsByTags(c("unimodal", "separable"))
#' @export
filterFunctionsByTags = function(tags) {
  assertSubset(tags, choices = getAvailableTags(), empty.ok = FALSE)

  fun.generators = getGeneratorFunctions()

  # filter by tags
  filtered.generators = Filter(function(fun) {
    fun.tags = attr(fun, "tags")
    return(BBmisc::isSubset(tags, fun.tags))
  }, fun.generators)

  # cleanup
  names = sapply(filtered.generators, function(fun) attr(fun, "name"))
  names(names) = NULL
  return(names)
}

# Get all generator objects.
#
# @return [function]
#   Vector of functions
getGeneratorFunctions = function() {
  # get all methods
  all.methods = ls("package:smoof")
  # get the function and not the names only
  all.methods = sapply(all.methods, get)
  # filter generators
  fun.generators = Filter(function(fun) inherits(fun, "smoof_generator"), all.methods)

  return(fun.generators)
}

getGeneratorByName = function(fun.name) {
  # get all methods of the package
  all.methods = sapply(ls("package:smoof"), get)
  fun.generator = Filter(function(fun) (fun.name %in% attr(fun, "name")), all.methods)
  if (length(fun.generator) == 0L) {
    return(NULL)
  }
  return(fun.generator[[1L]])
}
