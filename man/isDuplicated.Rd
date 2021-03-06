% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isDuplicated.R
\name{isDuplicated}
\alias{isDuplicated}
\title{isDuplicated}
\usage{
isDuplicated(data)
}
\arguments{
\item{x}{an object that is coerceable to a matrix containing raw SSR fragment lengths.}
}
\value{
a complex named list containing an element for each locus in the input data
and a sub-element for the output of each of SSRr's core functions
}
\description{
isDuplicated takes an object that is coercable to a matrix and returns a tibble of logical indicating
 wheather values for each observation are duplicates. The first column is exspected to be a primary key
 so it is returned unchanged.
}
