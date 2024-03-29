% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.cube.R
\name{makeCubeFunction}
\alias{makeCubeFunction}
\title{Cube Function}
\usage{
makeCubeFunction()
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Cube Function.

[\code{smoof_single_objective_function}]
}
\description{
The Cube Function is defined as follows:
\deqn{f(\mathbf{x}) = 100 (\mathbf{x}_2 - \mathbf{x}_1^3)^2 + (1 - \mathbf{x}_1)^2.}
The box-constraints are given by \eqn{\mathbf{x}_i \in [-10, 10], i = 1, 2.}
}
\references{
A. Lavi, T. P. Vogel (eds), Recent Advances in Optimization
Techniques, John Wliley & Sons, 1966.
}
