% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAvailableTags.R
\name{getAvailableTags}
\alias{getAvailableTags}
\title{Returns a character vector of possible function tags.}
\usage{
getAvailableTags()
}
\value{
[\code{character}]
 Character vector of all the possible tags
}
\description{
Test function are frequently distinguished by characteristic high-level properties,
e.g., uni-modal or multi-modal, continuous or discontinuous, separable or non-separable.
The \pkg{smoof} package offers the possibility to associate a set of properties,
termed \dQuote{tags} to a \code{smoof_function}. This helper function returns
a character vector of all possible tags.
}
