#' Check Element Equivalency
#'
#' Backend function, used by \code{d_adjRand}.
#'
#' @param a A factor.
#' @return A square matrix where \code{[i,j]=1} when
#'  \code{a[i] == a[j]}, or 0 otherwise.
ij_same_any <- function(a){

  # For each category, check if element i equals element j
  categories <- unique(a)
  ij_same_cats <- lapply(categories, function(cat){

    ij_same_cat <- t(t(a == cat)) %*% t(a == cat)
    ij_same_cat[is.na(ij_same_cat)] <- 0 # NAs never belong to a cluster
    return(ij_same_cat)
  })

  # Check if element i EVER equals element j
  ij_same_any <- Reduce(`|`, ij_same_cats) # TRUE if (i,j) ever the same
  ij_same_any * 1 # coerce boolean to numeric
}

#' Calculate Adjusted Rand Distance
#'
#' A factor metric that is categorical and relative.
#'
#' See Section 2.2.1 of Caruana et al. paper "Meta Clustering".
#'  This function sums I_ij for all pairs (i,j) where I_ij=1 if points i and j
#'  have the same label in one input but different labels in the other input.
#'  The total sum is then scaled by the total number of pairs, and so the
#'  metric will always range from 0 to 1.
#'
#' NAs have no category assignment. As such, an NA never equals an NA.
#'  Here, the distance between two vectors of NAs is 0 because neither vector
#'  has overlapping categories. The distance between a vector of NAs and a
#'  vector of a single category is 1 because one vector has no overlapping
#'  categories while the other does.
#'
#' @param a,b Factors.
#' @return The difference between \code{a} and \code{b}.
#' @export
d_adjRand <- function(a, b){

  A <- ij_same_any(a)
  B <- ij_same_any(b)

  # Count if two elements are same in one cluster but not other
  tri <- (A & !B) | (B & !A)

  # Divide by possible
  N <- nrow(tri)
  sum(tri[lower.tri(tri)]) / (N*(N-1)/2)
}

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
#'
#' @param a,b Factors.
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
#'
#' @param a,b Factors.
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
