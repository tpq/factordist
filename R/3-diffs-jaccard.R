#' Calculate Union Similarity
#'
#' A factor metric that is categorical and absolute.
#'
#' This function counts the number of times that an element of \code{a}
#'  equals an element of \code{b}. In other words, \code{sum(a==b)}.
#'
#' NAs have no category assignment. As such, an NA never equals an NA.
#'  Two vectors of NAs are maximally different because they never equal
#'  one another. A vector of NAs and a vector of a single category is
#'  also maximally different because they never equal one another.
#'  NAs are ignored in the denominator of Jaccard. So, the input
#'  \code{a=c("A", "A", NA, NA)} is the same as
#'  \code{b=c("A", "A", NA, NA)}.
#'
#' @param a,b A vector, typically a factor.
#' @return The similarity between \code{a} and \code{b}.
#' @export
s_union <- function(a, b){

  equality <- a == b
  equality[is.na(equality)] <- 0 # only one NA is disagreement
  sum(equality)
}

#' Calculate Jaccard Distance
#'
#' A factor metric that is categorical and absolute.
#'
#' This function measures the Jaccard distance between the elements of
#'  \code{a} and the elements of \code{b}. It normalizes the size of
#'  the set union by the size of the individual sets.
#'
#' NAs have no category assignment. As such, an NA never equals an NA.
#'  Two vectors of NAs are maximally different because they never equal
#'  one another. A vector of NAs and a vector of a single category is
#'  also maximally different because they never equal one another.
#'  NAs are ignored in the denominator of Jaccard. So, the input
#'  \code{a=c("A", "A", NA, NA)} is the same as
#'  \code{b=c("A", "A", NA, NA)}.
#'
#' @param a,b A vector, typically a factor.
#' @return The difference between \code{a} and \code{b}.
#' @export
d_jaccard <- function(a, b){

  union <- s_union(a, b)
  Anorm <- sum(!is.na(a))
  Bnorm <- sum(!is.na(b))
  d <- 1 - union / (Anorm + Bnorm - union)
  if(is.nan(d)) d <- 1
  d
}
