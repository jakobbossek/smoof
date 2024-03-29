% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mof.gomop.R
\name{makeGOMOPFunction}
\alias{makeGOMOPFunction}
\title{GOMOP function generator.}
\usage{
makeGOMOPFunction(dimensions = 2L, funs = list())
}
\arguments{
\item{dimensions}{[\code{integer(1)}]\cr
Size of corresponding parameter space.}

\item{funs}{[\code{list}]\cr
List of single-objective smoof functions.}
}
\value{
[\code{smoof_multi_objective_function}]
 Returns an instance of the GOMOP function as a \code{smoof_multi_objective_function} object.
}
\description{
Construct a multi-objective function by putting together multiple single-objective
smoof functions.
}
\details{
The decision space of the resulting function is restricted
to \eqn{[0,1]^d}. Each parameter \eqn{x} is stretched for each objective function.
I.e., if \eqn{f_1, \ldots, f_n} are the single objective smoof functions with
box constraints \eqn{[l_i, u_i], i = 1, \ldots, n}, then
\deqn{
  f(x) = \left(f_1(l_1 + x * (u_1 - l_1)), \ldots, f_1(l_1 + x * (u_1 - l_1))\right)
}
for \eqn{x \in [0,1]^d} where the additions and multiplication are performed
component-wise.
}
