#' Returns the global optimum and its value.
#'
#' @template arg_smoof_function
#' @return [\code{list}] List containing the following entries:
#' \itemize{
#'   \item{param [\code{list}]}{Named list of parameter value(s).}
#'   \item{value [\code{numeric(1)}]}{Optimal value.}
#' }
#' @note Keep in mind, that this method makes sense only for single-objective target function.
#' @export
getGlobalOptimum = function(fn) {
  UseMethod("getGlobalOptimum")
}

#' @export
getGlobalOptimum.smoof_single_objective_function = function(fn) {
  return(list(
    param = attr(fn, "global.opt.params"),
    value = attr(fn, "global.opt.value")
  ))
}

#' @export
getGlobalOptimum.smoof_multi_objective_function = function(fn) {
  stopf("No global optimum available for multi-objective function.")
}

#' @export
getGlobalOptimum.smoof_wrapped_function = function(fn) {
  return(getGlobalOptimum(getWrappedFunction(fn)))
}
