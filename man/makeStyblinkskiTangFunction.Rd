% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.styblinski.tang.R
\name{makeStyblinkskiTangFunction}
\alias{makeStyblinkskiTangFunction}
\title{Styblinkski-Tang function}
\usage{
makeStyblinkskiTangFunction()
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Styblinkski-Tang Function.

[\code{smoof_single_objective_function}]
}
\description{
This function is based on the definition
\deqn{f(\mathbf{x}) = \frac{1}{2} \sum_{i = 1}^{2} (\mathbf{x}_i^4 - 16 \mathbf{x}_i^2 + 5\mathbf{x}_i)}
with box-constraints given by \eqn{\mathbf{x}_i \in [-5, 5], i = 1, 2}.
}
\references{
Z. K. Silagadze, Finding Two-Dimesnional Peaks, Physics of
Particles and Nuclei Letters, vol. 4, no. 1, pp. 73-80, 2007.
}
