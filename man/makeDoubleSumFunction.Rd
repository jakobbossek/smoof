% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.double.sum.R
\name{makeDoubleSumFunction}
\alias{makeDoubleSumFunction}
\title{Double-Sum Function}
\usage{
makeDoubleSumFunction(dimensions)
}
\arguments{
\item{dimensions}{[\code{integer(1)}]\cr
Size of corresponding parameter space.}
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Double-Sum Function.

[\code{smoof_single_objective_function}]
}
\description{
Also known as the rotated hyper-ellipsoid function. The formula is given by
\deqn{f(\mathbf{x}) = \sum_{i=1}^n \left( \sum_{j=1}^{i} \mathbf{x}_j \right)^2}
with \eqn{\mathbf{x}_i \in [-65.536, 65.536], i = 1, \ldots, n}.
}
\references{
H.-P. Schwefel. Evolution and Optimum Seeking.
John Wiley & Sons, New York, 1995.
}
