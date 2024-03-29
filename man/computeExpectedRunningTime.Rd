% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeExpectedRunningTime.R
\name{computeExpectedRunningTime}
\alias{computeExpectedRunningTime}
\title{Compute the Expected Running Time (ERT) performance measure.}
\usage{
computeExpectedRunningTime(
  fun.evals,
  fun.success.runs = NULL,
  fun.reached.target.values = NULL,
  fun.target.value = NULL,
  penalty.value = Inf
)
}
\arguments{
\item{fun.evals}{[\code{numeric}]\cr
Vector containing the number of function evaluations.}

\item{fun.success.runs}{[\code{logical}]\cr
Boolean vector indicating which algorithm runs were successful,
i.e., which runs reached the desired target value. Default is \code{NULL}.}

\item{fun.reached.target.values}{[\code{numeric} | \code{NULL}]\cr
Numeric vector with the objective values reached in the runs. Default is
\code{NULL}.}

\item{fun.target.value}{[\code{numeric(1)} | \code{NULL}]\cr
Target value which shall be reached. Default is \code{NULL}.}

\item{penalty.value}{[\code{numeric(1)}]\cr
Penalty value which should be returned if none of the algorithm runs
was successful. Default is \code{Inf}.}
}
\value{
[\code{numeric(1)}]
  Estimated Expected Running Time.
}
\description{
The functions can be called in two different ways
\itemize{
  \item{1. Pass a vector of function evaluations and a logical vector which
  indicates which runs were successful (see details).}
  \item{2. Pass a vector of function evaluation, a vector of reached target
  values and a single target value. In this case the logical vector of
  option 1. is computed internally.}
}
}
\details{
The Expected Running Time (ERT) is one of the most popular performance
measures in optimization. It is defined as the expected number of function
evaluations needed to reach a given precision level, i.e., to reach a certain
objective value for the first time.
}
\references{
A. Auger and N. Hansen. Performance evaluation of an advanced local
search evolutionary algorithm. In Proceedings of the IEEE Congress on Evolutionary
Computation (CEC 2005), pages 1777-1784, 2005.
}
