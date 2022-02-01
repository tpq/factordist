#' Symmetrize Matrix
#'
#' Symmetrize a square matrix by copying the lower
#'  triangle to the upper triangle.
#'
#' @param mat A square matrix.
#' @return A square matrix.
#' @export
sym <- function(mat){

  if(nrow(mat) != ncol(mat)) stop("The input must be a square matrix.")
  replacement <- mat[lower.tri(mat)]
  mat_t <- t(mat)
  mat_t[lower.tri(mat_t)] <- replacement
  mat_t
}

#' Symmetrize Matrix
#'
#' Symmetrize a square matrix, but use the smallest
#'  of the lower and upper triangle values.
#'
#' @param mat A square matrix.
#' @return A square matrix.
#' @export
sym_min <- function(mat){

  if(nrow(mat) != ncol(mat)) stop("The input must be a square matrix.")
  replacement <- mat[lower.tri(mat)]
  mat_t <- t(mat)
  original <- mat_t[lower.tri(mat_t)]
  mat_t[lower.tri(mat_t)] <- pmin(replacement, original)
  sym(mat_t)
}

#' Symmetrize Matrix
#'
#' Symmetrize a square matrix, but use the largest
#'  of the lower and upper triangle values.
#'
#' @param mat A square matrix.
#' @return A square matrix.
#' @export
sym_max <- function(mat){

  if(nrow(mat) != ncol(mat)) stop("The input must be a square matrix.")
  replacement <- mat[lower.tri(mat)]
  mat_t <- t(mat)
  original <- mat_t[lower.tri(mat_t)]
  mat_t[lower.tri(mat_t)] <- pmax(replacement, original)
  sym(mat_t)
}
