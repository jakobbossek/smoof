#' @title Smoof function
#'
#' @description
#' Regular R function with additional classes \code{smoof_function} and one of \code{smoof_single_objective_function}
#' or code{smoof_multi_objective_function}. Both single- and multi-objective functions share the following attributes.
#' \describe{
#'   \item{name [\code{character(1)}]}{Optional function name.}
#'   \item{id [\code{character(1)}]}{Short identifier.}
#'   \item{description [\code{character(1)}]}{Optional function description.}
#'   \item{has.simple.signature}{\code{TRUE} if the target function expects a vector as input and \code{FALSE}
#'   if it expects a named list of values.}
#'   \item{par.set [\code{\link[ParamHelpers]{ParamSet}}]}{Parameter set describing different ascpects of the target function parameters, i. e.,
#'   names, lower and/or upper bounds, types and so on.}
#'   \item{n.objectives [\code{integer(1)}]}{Number of objectives.}
#'   \item{noisy [\code{logical(1)}]}{Boolean indicating whether the function is noisy or not.}
#'   \item{fn.mean [\code{function}]}{Optional true mean function in case of a noisy objective function.}
#'   \item{minimize [\code{logical(1)}]}{Logical vector of length \code{n.objectives} indicating which objectives shall
#'   be minimized/maximized.}
#'   \item{vectorized [\code{logical(1)}]}{Can the handle \dQuote{vector} input, i. e., does it accept matrix of
#'   parameters?}
#'   \item{constraint.fn [\code{function}]}{Optional function which returns a logical vector with each component indicating
#'   whether the corresponding constraint is violated.}
#' }
#'
#' Furthermore, single-objective function may contain additional parameters with information on
#' local and/or global optima as well as characterizing tags.
#' \describe{
#'   \item{tags [\code{character}]}{Optional character vector of tags or keywords.}
#'   \item{global.opt.params [\code{data.frame}]}{Data frame of parameter values of global optima.}
#'   \item{global.opt.value [\code{numeric(1)}]}{Function value of global optima.}
#'   \item{local.opt.params [\code{data.frame}]}{Data frame of parameter values of local optima.}
#'   \item{global.opt.value [\code{numeric}]}{Function values of local optima.}
#' }
#'
#' Currently tagging is not possible for multi-objective functions. The only additional attribute may be
#' a reference point:
#' \describe{
#'   \item{ref.point [\code{numeric}]}{Optional reference point of length \code{n.objectives}}.
#' }
#'
#' @name smoof_function
#' @rdname smoof_function
#' @aliases smoof_single_objective_function smoof_multi_objective_function
NULL
