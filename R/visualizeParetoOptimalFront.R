#' Pareto-optimal front visualization.
#'
#' Quickly visualize the Pareto-optimal front of a bi-criteria objective
#' function by generating a factorial desing in the decision space, computing the
#' objective values and filtering out the dominated points. Helpful to get a
#' notion of how the front is shaped and how the distribution of the points
#' on the shape looks like.
#'
#' @param fn [\code{smoof_multi_objective_function}]\cr
#'   Multi-objective smoof function.
#' @param length.out [\code{integer(1)}]\cr
#'   A grid is generated in the decision space. This parameter determines how many
#'   points are chosen for each dimension. Higher values lead to more dense
#'   decision and objective space. Default is 100.
#' @param show.only.front [\code{logical(1)}]\cr
#'   Should only the approximated front be plotted or all abjective values?
#'   Default is \code{FALSE}.
#' @param limits.by.front [\code{logical(1)}]\cr
#'   Should the xlim and ylim values for the plot be determined by the minimal
#'   and maximal value of the front? Default is \code{TRUE}. Usually you will
#'   get warnings in this case, since a lot of objective values will be \dQuote{
#'   out of bounds}.
#' @examples
#' # Here we visualize the Pareto-optimal front of the bi-objective ZDT3 function
#' # without "zooming in" to the nondominated points
#' fn = makeZDT3Function(dimensions = 2L)
#' vis = visualizeParetoOptimalFront(fn, limits.by.front = FALSE)
#'
#' # Again, but now with zooming in to get a better notion of the front shape
#' vis = visualizeParetoOptimalFront(fn, limits.by.front = TRUE)
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
visualizeParetoOptimalFront = function(fn, length.out = 100, show.only.front = FALSE, limits.by.front = TRUE) {
    n.objectives = getNumberOfObjectives(fn)
    if (n.objectives != 2L) {
        stopf("Visualization only availale for bi-objective function, but the provided function has %i", n.objectives)
    }
    requirePackages("emoa", why = "smoof::visualizeParetoOptimalFront")

    par.set = getParamSet(fn)
    lower = getLower(par.set)
    upper = getUpper(par.set)
    x.seq = seq(lower[1], upper[1], length.out = length.out)
    y.seq = seq(lower[2], upper[2], length.out = length.out)
    # make factorial design in the decision space
    gr = expand.grid(x.seq, y.seq)

    # get the objective points
    points = apply(gr, 1, fn)

    # filter nondominated points
    eff.points = emoa::nondominated_points(points)

    # transform to ggplot-friendly format
    points = as.data.frame(t(points))
    eff.points = as.data.frame(t(eff.points))
    colnames(points) = colnames(eff.points) = c("f1", "f2")

    pl = ggplot(mapping = aes_string(x = "f1", y = "f2"))
    if (!show.only.front) {
        pl = pl + geom_point(data = points)
    }
    pl = pl + geom_point(data = eff.points, colour = "tomato")
    if (limits.by.front) {
        limits = apply(eff.points, 2, range)
        pl = pl + xlim(limits[, 1]) + ylim(limits[, 2])
    }
    pl = pl + xlab(expression(f[1])) + ylab(expression(f[2]))
    pl = pl + ggtitle(sprintf("Objective space with approximative Pareto-optimal\n front for the bi-criteria %s function", getName(fn)))
    return(pl)
}
