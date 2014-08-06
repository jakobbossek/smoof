#' Generates non-noisy test function from soo function.
#'
#' @param id [\code{character(1)}]\cr
#'   Function id of SOO function from package soobench.
#' @param ... [\code{list}]\cr
#'   Further parameters passed to soo function generator. See the different
#'   soo functions for examples. Probably the most important one is 'dimensions'.
#' @return [\code{otf_function}]
#' @examples \dontrun{
#'   branin.fn = makeSingleObjectiveFunctionFromSOOFunction("branin")
#'   print(branin.fn)
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