#' @title
#' Generate smoof function by passing a character vector of generator
#' names.
#'
#' @description
#' This function is especially useful in combination with
#' \code{\link{filterFunctionsByTags}} to generate a test set of functions
#' with certain properties, e.~g., multimodality.
#'
#' @param fun.names [\code{character}]\cr
#'   Non empty character vector of generator function names.
#' @param ... [any]\cr
#'   Further arguments passed to generator.
#' @return [\code{smoof_function}]
#' @examples
#' # generate a testset of multimodal 2D functions
#' \dontrun{
#' test.set = makeFunctionsByName(filterFunctionsByTags("multimodal"), dimensions = 2L, m = 5L)
#' }
#' @seealso \code{\link{filterFunctionsByTags}}
#' @export
makeFunctionsByName = function(fun.names, ...) {
  assertCharacter(fun.names, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  fun.generators = getGeneratorFunctions()
  valid.fun.names = sapply(fun.generators, function(generator) attr(generator, "name"))

  # get function names of generators
  generator.calls = names(valid.fun.names)

  funs = lapply(fun.names, function(fun.name) {
    if (fun.name %nin% valid.fun.names) {
      stopf("There is no generator for function '%s'.", fun.name)
    }

    # find the right generator
    the.generator.id = which(fun.name == valid.fun.names)
    generator = get(generator.calls[the.generator.id])

    # Some funs expect a dimension attribute, others do not
    # 1. if dimension = 2 is passed and fun does not expect dimension attribute,
    #    we simply call it without it.
    # 2. if dimension > 2 is passed we throw an error
    args = list(...)
    # delete arguments that are not relevant for the function
    names.form.args = names(formals(generator))
    if (is.null(names.form.args))
      names.form.args = c()
    names.args = names(args)
    args = args[intersect(names.args, names.form.args)]

    if (is.null(args$dimensions)) {
      if ("scalable" %in% attr(generator, "tags")) {
        stopf("'%s' is scalable and needs a dimension argument to be passed.", fun.name)
      }
      return(do.call(generator, args))
    } else {
      if ("scalable" %in% attr(generator, "tags")) {
        tryres = try({do.call(generator, args)}, silent = TRUE)
        if (inherits(tryres, "try-error")) {
          warningf("Function '%s' could not be generated.", fun.name)
          return(NA)
        }
        return(tryres)
      } else if (args$dimensions == 2L) {
        args$dimensions = NULL
        return(do.call(generator, args))
      } else {
        warningf("Dimension attribute passed, but '%s' is a non-scalable function.", fun.name)
        return(NA)
      }
    }
  })
  funs = funs[!is.na(funs)]
  return(funs)
}
