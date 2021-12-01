#' Apply Functions Over Pairs of Vectors
#'
#' This function applies another function over pairs of rows
#'  or columns. It treats each row (or column) as a vector,
#'  then applies a function to each row pair (or column).
#'  For i,j in 1...N rows (or columns), it returns an N by N matrix
#'  containing the result of FUN(Xi, Xj).
#'
#' @param X A matrix.
#' @param MARGIN Choose from 1 or 2. If 1, apply function to
#'  pairs of rows. If 2, apply function to pairs of columns.
#' @param FUN The function to apply to row or column pairs.
#'  The function should take two vectors as input.
#' @param lower_only A boolean. Toggles whether to apply
#'  function to lower triangle pairs only.
#' @param ... Arguments passed to \code{FUN}.
#' @return A square matrix.
#'
#' @examples
#' library(factordist)
#' x <- simulate_factor_data(30, 6)
#' mat <- papply(x, 2, d_adjRand)
#'
#' @export
papply <- function(X, MARGIN = 2, FUN, lower_only = FALSE, ...){

  if(MARGIN == 1){
    X <- t(X)
  }

  args <- as.list(substitute(list(...)))[-1]

  # Apply difference function
  D = matrix(0, ncol(X), ncol(X))
  for(i in 1:ncol(X)){
    numTicks <- progress(i, ncol(X), numTicks)
    for(j in 1:ncol(X)){
      if(lower_only){
        if(j<i){
          D[i,j] <- do.call(FUN, append(list(X[,i], X[,j]), args))
        }
      }else{
        D[i,j] <- do.call(FUN, append(list(X[,i], X[,j]), args))
      }
    }
  }

  # Name results
  rownames(D) <- colnames(X)
  colnames(D) <- colnames(X)
  return(D)
}
