#' Get a list of implemented test functions with specific tags.
#'
#' @param tags [\code{character}]\cr
#'   Character vector of tags.
#' @return [\code{character}]
#'   Named vector of function names with the given tags.
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
