% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.exponential.R
\name{makeExponentialFunction}
\alias{makeExponentialFunction}
\title{Exponential Function}
\usage{
makeExponentialFunction(dimensions)
}
\arguments{
\item{dimensions}{[\code{integer(1)}]\cr
Size of corresponding parameter space.}
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Exponential Function.

[\code{smoof_single_objective_function}]
}
\description{
This scalable test function is based on the definition
\deqn{f(\mathbf{x}) = -\exp\left(-0.5 \sum_{i = 1}^{n} \mathbf{x}_i^2\right)}
with the box-constraints \eqn{\mathbf{x}_i \in [-1, 1], i = 1, \ldots, n}.
}
\references{
S. Rahnamyan, H. R. Tizhoosh, N. M. M. Salama, Opposition-Based
Differential Evolution (ODE) with Variable Jumping Rate, IEEE Sympousim
Foundations Com- putation Intelligence, Honolulu, HI, pp. 81-88, 2007.
}
