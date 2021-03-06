% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findVal.R
\name{findVal}
\alias{findVal}
\title{findVal}
\usage{
findVal(frag, lim, FUN, ...)
}
\arguments{
\item{frag}{a vector of microsatilite fragment length}

\item{lim}{A vector of bin limits}

\item{FUN}{a function to be applied to each bin returning a value for each fragment}

\item{...}{additional arguments to be supplied to FUN}
}
\value{
a list containing results of bandwidth estimation, KDE, bin limits, central values, mean and frequency.
}
\description{
applies a function to each bin returning a value for each fragment reading
}
