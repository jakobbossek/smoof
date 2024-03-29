% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.brown.R
\name{makeBrownFunction}
\alias{makeBrownFunction}
\title{Brown Function}
\usage{
makeBrownFunction(dimensions)
}
\arguments{
\item{dimensions}{[\code{integer(1)}]\cr
Size of corresponding parameter space.}
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Brown Function.

[\code{smoof_single_objective_function}]
}
\description{
This function belongs the the uni-modal single-objective test functions. The
function is forumlated as
\deqn{f(\mathbf{x}) = \sum_{i = 1}^{n} (\mathbf{x}_i^2)^{(\mathbf{x}_{i + 1} + 1)} + (\mathbf{x}_{i + 1})^{(\mathbf{x}_i + 1)}}
subject to \eqn{\mathbf{x}_i \in [-1, 4]} for \eqn{i = 1, \ldots, n}.
}
\references{
O. Begambre, J. E. Laier, A hybrid Particle Swarm Optimization -
Simplex Algorithm (PSOS) for Structural Damage Identification, Journal of
Advances in Engineering Software, vol. 40, no. 9, pp. 883-891, 2009.
}
