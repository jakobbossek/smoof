% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeObjectiveFunction.R
\name{makeObjectiveFunction}
\alias{makeObjectiveFunction}
\title{Internal generator for function of smoof type.}
\usage{
makeObjectiveFunction(
  name = NULL,
  id = NULL,
  description = NULL,
  fn,
  has.simple.signature = TRUE,
  par.set,
  n.objectives = NULL,
  noisy = FALSE,
  fn.mean = NULL,
  minimize = NULL,
  vectorized = FALSE,
  constraint.fn = NULL
)
}
\arguments{
\item{name}{[\code{character(1)}]\cr
Optional function name used e.g. in plots.}

\item{id}{[\code{character(1)}]\cr
Optional identifier for the function}

\item{description}{[\code{character(1)} | \code{NULL}]\cr
Optional function description.}

\item{fn}{[\code{function}]\cr
Target function.}

\item{has.simple.signature}{[\code{logical(1)}]\cr
Set this to \code{TRUE} if the target function expects a vector as input and \code{FALSE}
if it expects a named list of values. The latter is needed if the function depends on mixed
parameters. Default is \code{TRUE}.}

\item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
Parameter set describing different aspects of the target function parameters, i. e.,
names, lower and/or upper bounds, types and so on. See \code{\link[ParamHelpers]{makeParamSet}}
for further information.}

\item{n.objectives}{[\code{integer(1)}]\cr
Number of objectives of the multi-objective function.}

\item{noisy}{[\code{logical(1)}]\cr
Is the function noisy? Defaults to \code{FALSE}.}

\item{fn.mean}{[\code{function}]\cr
Optional true mean function in case of a noisy objective function. This functions should
have the same mean as \code{fn}.}

\item{minimize}{[\code{logical}]\cr
Logical vector of length \code{n.objectives} indicating which objectives shall
be minimized/maximized.
The default is \code{TRUE} \code{n.objectives} times.}

\item{vectorized}{[\code{logical(1)}]\cr
Can the handle \dQuote{vector} input, i. e., does it accept matrix of
parameters? Default is \code{FALSE}.}

\item{constraint.fn}{[\code{function | NULL}]\cr
Function which returns a logical vector indicating which indicates whether certain conditions
are met or not. Default is \code{NULL}, which means, that there are no constraints (beside possible)
box constraints defined via the \code{par.set} argument.}
}
\value{
[\code{function}] Target function with additional stuff attached as attributes.
}
\description{
Internal generator for function of smoof type.
}
