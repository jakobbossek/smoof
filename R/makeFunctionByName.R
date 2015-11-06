#' Convenience function to generate a smoof function by passing the function name
#' as a string.
#'
#' @param fun.name [\code{character(1)}]\cr
#'   Name of the generator function as a string or a list of generator function names.
#' @param ... [any]\cr
#'   Further arguments passed to generator.
#' @return [\code{smoof_function}]
#' @export
makeFunctionByName = function(fun.name, ...) {
  UseMethod("makeFunctionByName")
}

#' @export
makeFunctionByName.character = function(fun.name, ...) {
  assertString(fun.name, na.ok = FALSE)
  fun.generators = getGeneratorFunctions()
  valid.fun.names = sapply(fun.generators, function(generator) attr(generator, "name"))
  #print(valid.fun.names)
  if (fun.name %nin% valid.fun.names) {
    stopf("There is no generator for function '%s'.", fun.name)
  }

  # get function names of generators
  generator.calls = names(valid.fun.names)

  # find the right generator
  the.generator.id = which(fun.name == valid.fun.names)
  generator = get(generator.calls[the.generator.id])

  # Some funs expect a dimension attribute, others do not
  # 1. if dimension = 2 is passed and fun does not expect dimension attribute,
  #    we simply call it without it.
  # 2. if dimension > 2 is passed we throw an error
  args = list(...)
  if (is.null(args$dimensions)) {
    if ("scalable" %in% attr(generator, "tags")) {
      stopf("'%s' is scalable and needs a dimension argument to be passed.", fun.name)
    }
    return(do.call(generator, list()))
  } else {
    if ("scalable" %in% attr(generator, "tags")) {
      return(do.call(generator, args))
    } else if (args$dimensions == 2L) {
      return(do.call(generator, list()))
    } else {
      stopf("Dimension > 3 passed, but '%s' is a non-scalable 2D function.", fun.name)
    }
  }
}

#' @export
makeFunctionByName.list = function(fun.name, ...) {
  assertList(fun.name, types = "character", any.missing = FALSE, all.missing = FALSE, min.len = 1L)
  return(lapply(fun.name, function(x) makeFunctionByName(x, ...)))
}
