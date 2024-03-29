% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeMultiObjectiveFunction.R
\name{makeMultiObjectiveFunction}
\alias{makeMultiObjectiveFunction}
\title{Generator for multi-objective target functions.}
\usage{
makeMultiObjectiveFunction(
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
  constraint.fn = NULL,
  ref.point = NULL
)
}
\arguments{
\item{name}{[\code{character(1)}]\cr
Function name. Used for the title of plots for example.}

\item{id}{[\code{character(1)} | \code{NULL}]\cr
Optional short function identifier. If provided, this should be a short
name without whitespaces and now special characters beside the underscore.
Default is \code{NULL}, which means no ID at all.}

\item{description}{[\code{character(1)} | \code{NULL}]\cr
Optional function description.}

\item{fn}{[\code{function}]\cr
Objective function.}

\item{has.simple.signature}{[\code{logical(1)}]\cr
Set this to \code{TRUE} if the objective function expects a vector as input and \code{FALSE}
if it expects a named list of values. The latter is needed if the function depends on mixed
parameters. Default is \code{TRUE}.}

\item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
Parameter set describing different aspects of the objective function parameters, i.~e.,
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
Logical vector of length \code{n.objectives} indicating if the corresponding
objectives shall be minimized or maximized.
Default is the vector with all components set to \code{TRUE}.}

\item{vectorized}{[\code{logical(1)}]\cr
Can the objective function handle \dQuote{vector} input, i.~e., does it
accept matrix of parameters? Default is \code{FALSE}.}

\item{constraint.fn}{[\code{function | NULL}]\cr
Function which returns a logical vector indicating whether certain conditions
are met or not. Default is \code{NULL}, which means, that there are no constraints
beside possible box constraints defined via the \code{par.set} argument.}

\item{ref.point}{[\code{numeric}]\cr
Optional reference point in the objective space, e.g., for hyper-volume computation.}
}
\value{
[\code{function}] Target function with additional stuff attached as attributes.
}
\description{
Generator for multi-objective target functions.
}
\examples{
fn = makeMultiObjectiveFunction(
  name = "My test function",
  fn = function(x) c(sum(x^2), exp(x)),
  n.objectives = 2L,
  par.set = makeNumericParamSet("x", len = 1L, lower = -5L, upper = 5L)
)
print(fn)
}
