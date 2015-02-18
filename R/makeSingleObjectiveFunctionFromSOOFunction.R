#' Generates non-noisy test function from single objective benchmarking function.
#'
#' @param id [\code{character(1)}]\cr
#'   Function id of single objective benchmarking function from package \pkg{soobench}.
#' @param ... [\code{list}]\cr
#'   Further parameters passed to soo function generator. See the different
#'   soo functions for examples. Probably the most important one is 'dimensions'.
#' @return [\code{otf_function}]
#' @examples \dontrun{
#'   branin.fn = makeSingleObjectiveFunctionFromSOOFunction("branin")
#'   print(branin.fn)
#'   ackley.fn.5d = makeSingleObjectiveFunctionFromSOOFunction("ackley", dimensions = 5L)
#' }
#' @export
makeSingleObjectiveFunctionFromSOOFunction = function(id, ...) {
	generator = paste(id, "function", sep = "_")
	soo.fn = do.call(generator, args = list(...))
	raw.soo.fn = soo.fn
	attributes(raw.soo.fn) = NULL
	makeSingleObjectiveFunction(
		name = function_name(soo.fn),
		par.set = extractParamSetFromSooFunction(soo.fn),
		fn = raw.soo.fn
	)
}