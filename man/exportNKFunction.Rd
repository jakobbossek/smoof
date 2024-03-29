% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeNKFunction.R
\name{exportNKFunction}
\alias{exportNKFunction}
\alias{importNKFunction}
\title{Export/import (rM)NK-landscapes}
\usage{
exportNKFunction(x, path)

importNKFunction(path)
}
\arguments{
\item{x}{[\code{smoof_function}]\cr
NK-landscape of rMNK-landscape.}

\item{path}{[\code{character(1)}]\cr
Path to file.}
}
\value{
Silently returns \code{TRUE} on success.
}
\description{
NK-landscapes and rMNK-landscapes are randomly generated combinatorial structures.
In contrast to continuous benchmark function it thus makes perfect sense to store
the landscape definitions in a text-based format.
}
\details{
The format uses two comment lines with basic information like the package version,
the date of storage etc. The third line contains \eqn{\rho}, \eqn{M} and \eqn{N}
separated by a single-whitespace. Following that follow epistatic links from
which the number of epistatic links can be attracted. There are \eqn{M * N}
lines for a MNK-landscape with \eqn{M} objectives and input dimension \code{N}.
The first \eqn{N} lines contain the links for the first objective an so on.
Following that the tabular values follow in the same manner. For every position
\eqn{i = 1, \ldots, N} there is a line with \eqn{2^{K_i + 1}} values.

Note: \code{exportNKFunction} overwrites existing files without asking.
}
\seealso{
importNKFunction

Other nk_landscapes: 
\code{\link{makeMNKFunction}()},
\code{\link{makeNKFunction}()}
}
\concept{nk_landscapes}
