% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-diffs-relAcc.R
\name{s_relAcc}
\alias{s_relAcc}
\title{Calculate Relative Accuracy}
\usage{
s_relAcc(a, b, return_summary = FALSE)
}
\arguments{
\item{a, b}{A vector, typically a factor.}

\item{return_summary}{A logical. Toggles whether the function
should return all overlap scores used to measure accuracy,
or just the final accuracy.}
}
\value{
The accuracy between \code{a} and \code{b}.
}
\description{
A factor metric that is categorical and relative.
}
\details{
This calculates a kind of accuracy between two vectors. Unlike true accuracy,
 relative accuracy does not assume that the labels in input \code{a} will necessarily
 be the same as the labels in input \code{b}. For example, the input
 \code{a=c("A", "A", "B", "B")} is same as the input \code{b=c(1, 1, 2, 2)}
 because every "A" corresponds to a 1 and every "B" corresponds to a 2.

NAs have no category assignment. As such, an NA never equals an NA.
 Here, the accuracy between two vectors of NAs is 0 because neither vector
 has overlapping categories. The accuracy between a vector of NAs and a
 vector of a categories is also 0 because again there is no overlap.
 NAs are ignored in the denominator of accuracy. So, the input
 \code{a=c("A", "A", NA, NA)} is the same as
 \code{b=c("B", "B", NA, NA)}.
}
