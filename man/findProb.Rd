% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findProb.R
\name{findProb}
\alias{findProb}
\title{findProb}
\usage{
findProb(frag, lim)
}
\arguments{
\item{frag}{a vector of fragment lengths}

\item{lim}{a vector of bin limits}
}
\value{
p value of each fragment. NA for unique bins.
}
\description{
retuns the probability of observing each fragment given the other fragments in that bin assuming normal distribution.
}
