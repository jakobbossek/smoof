% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizeParetoOptimalFront.R
\name{visualizeParetoOptimalFront}
\alias{visualizeParetoOptimalFront}
\title{Pareto-optimal front visualization.}
\usage{
visualizeParetoOptimalFront(fn, ...)
}
\arguments{
\item{fn}{[\code{smoof_multi_objective_function}]\cr
Multi-objective smoof function.}

\item{...}{[any]\cr
Arguments passed to \code{\link[mco]{nsga2}}.}
}
\value{
[\code{\link[ggplot2]{ggplot}}]
Returns a ggplot object representing the Pareto-optimal front visualization.
}
\description{
Quickly visualize the Pareto-optimal front of a bi-criteria objective
function by calling the EMOA \code{\link[mco]{nsga2}} and extracting the
approximated Pareto-optimal front.
}
\examples{
# Here we visualize the Pareto-optimal front of the bi-objective ZDT3 function
fn = makeZDT3Function(dimensions = 3L)
vis = visualizeParetoOptimalFront(fn)

# Alternatively we can pass some more algorithm parameters to the NSGA2 algorithm
vis = visualizeParetoOptimalFront(fn, popsize = 1000L)

}
