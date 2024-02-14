#' @title
#' Pareto-optimal front visualization.
#'
#' @description
#' Quickly visualize the Pareto-optimal front of a bi-criteria objective
#' function by calling the EMOA \code{\link[mco]{nsga2}} and extracting the
#' approximated Pareto-optimal front.
#'
#' @param fn [\code{smoof_multi_objective_function}]\cr
#'   Multi-objective smoof function.
#' @param ... [any]\cr
#'   Arguments passed to \code{\link[mco]{nsga2}}.
#' @examples
#' # Here we visualize the Pareto-optimal front of the bi-objective ZDT3 function
#' fn = makeZDT3Function(dimensions = 3L)
#' vis = visualizeParetoOptimalFront(fn)
#'
#' # Alternatively we can pass some more algorithm parameters to the NSGA2 algorithm
#' vis = visualizeParetoOptimalFront(fn, popsize = 1000L)
#' 
#' @return [\code{\link[ggplot2]{ggplot}}]
#' Returns a ggplot object representing the Pareto-optimal front visualization.
#' 
#' @export
visualizeParetoOptimalFront = function(fn, ...) {
  n.objectives = getNumberOfObjectives(fn)
  if (!isMultiobjective(fn)) {
    BBmisc::stopf("Visualization of approximated Pareto-optimal front only possible fo multi-objective
      functions with two objectives at the moment.")
  }

  if (!requireNamespace("mco", quietly = TRUE))
    BBmisc::stopf("Package \"mco\" needed for this function to work.")

  par.set = ParamHelpers::getParamSet(fn)

  # get approximated Pareto front
  res = mco::nsga2(fn,
    idim = getNumberOfParameters(fn),
    odim = n.objectives,
    lower.bounds = ParamHelpers::getLower(par.set),
    upper.bounds = ParamHelpers::getUpper(par.set),
    ...
  )
  eff.points = res$value[res$pareto.optimal, ]

  # transform to ggplot-friendly format
  eff.points = as.data.frame(eff.points)
  colnames(eff.points) = c("f1", "f2")

  pl = ggplot2::ggplot(eff.points, mapping = ggplot2::aes_string(x = "f1", y = "f2"))
  pl = pl + ggplot2::geom_line(colour = "darkgray")
  pl = pl + ggplot2::xlab(expression(f[1])) + ggplot2::ylab(expression(f[2]))
  pl = pl + ggplot2::ggtitle(sprintf("Objective space with shape of Pareto-optimal\n
    front for the bi-criteria %s", getName(fn)))
  return(pl)
}
