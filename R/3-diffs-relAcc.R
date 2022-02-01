#' Calculate Relative Accuracy
#'
#' A factor metric that is categorical and relative.
#'
#' This calculates a kind of accuracy between two vectors. Unlike true accuracy,
#'  relative accuracy does not assume that the labels in input \code{a} will necessarily
#'  be the same as the labels in input \code{b}. For example, the input
#'  \code{a=c("A", "A", "B", "B")} is same as the input \code{b=c(1, 1, 2, 2)}
#'  because every "A" corresponds to a 1 and every "B" corresponds to a 2.
#'
#' NAs have no category assignment. As such, an NA never equals an NA.
#'  Here, the accuracy between two vectors of NAs is 0 because neither vector
#'  has overlapping categories. The accuracy between a vector of NAs and a
#'  vector of a categories is also 0 because again there is no overlap.
#'  NAs are ignored in the denominator of accuracy. So, the input
#'  \code{a=c("A", "A", NA, NA)} is the same as
#'  \code{b=c("B", "B", NA, NA)}.
#'
#' @param a,b A vector, typically a factor.
#' @param return_summary A logical. Toggles whether the function
#'  should return all overlap scores used to measure accuracy,
#'  or just the final accuracy.
#' @return The accuracy between \code{a} and \code{b}.
#' @export
s_relAcc <- function(a, b, return_summary = FALSE){

  confMat <- table(a, b)
  smallest_k <- pmin(ncol(confMat), nrow(confMat))
  res <- vector("list", length = smallest_k)
  if(smallest_k > 0){

    # Build confusion matrix that gives best possible agreement
    for(event in 1:smallest_k){
      rowmaxes <- apply(confMat, 1, max)
      rowmax <- which_max(rowmaxes)
      colmaxes <- apply(confMat[rowmax, , drop = FALSE], 2, max)
      colmax <- which_max(colmaxes)
      res[[event]] <-
        data.frame(
          "label1" = names(rowmax),
          "label2" = names(colmax),
          "overlap" = confMat[rowmax, colmax, drop = TRUE]
        )
      confMat <- confMat[-rowmax, -colmax, drop = FALSE]
    }

  }else{

    # Handle NA input
    res <- list(data.frame("label1" = NA, "label2" = NA, overlap = 0))
    acc <- 0
  }

  # Compute relative accuracy as (AGREEMENT SIZE) / (TOTAL SIZE)
  maxAgreements <- do.call("rbind", res)
  denominator <- length(a) - sum(is.na(a) & is.na(b)) # ignore double-NAs
  acc <- sum(maxAgreements$overlap) / denominator
  if(is.nan(acc)) acc <- 0
  if(return_summary) return(maxAgreements)
  acc
}
