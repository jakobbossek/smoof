% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sof.shekel.R
\name{makeShekelFunction}
\alias{makeShekelFunction}
\title{Shekel functions}
\usage{
makeShekelFunction(m)
}
\arguments{
\item{m}{[\code{numeric(1)}]\cr
Integer parameter (defines the number of local optima).
Possible values are 5, 7 or 10.}
}
\value{
An object of class \code{SingleObjectiveFunction}, representing the Shekel Functions.

[\code{smoof_single_objective_function}]
}
\description{
Single-objective test function based on the formula
\deqn{f(\mathbf{x}) = -\sum_{i=1}^{m} \left(\sum_{j=1}^{4} (x_j - C_{ji})^2 + \beta_{i}\right)^{-1}}.
Here, \eqn{m \in \{5, 7, 10\}} defines the number of local optima, \eqn{C} is a \eqn{4 x 10} matrix
and \eqn{\beta = \frac{1}{10}(1, 1, 2, 2, 4, 4, 6, 3, 7, 5, 5)} is a vector. See \url{https://www.sfu.ca/~ssurjano/shekel.html}
for a definition of \eqn{C}.
}
