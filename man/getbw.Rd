% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getbw.R
\name{getbw}
\alias{getbw}
\title{getbw}
\usage{
getbw(frag)
}
\arguments{
\item{frag}{a vector of fragment lengths}
}
\value{
optimum bandwidth for the distribution of fragment lengths
}
\description{
A wrapper for optim. Finds the optimum bandwidth by maximising the error lineLength.
}
