resample <- function(x, ...){

  x[sample.int(length(x), ...)]
}

rmax <- function(x){

  resample(which(x == max(x)))[1]
}

mode <- function(a){

  counts <- sort(table(a), decreasing = TRUE)
  maxes <- counts[counts == max(counts)]
  names(rmax(maxes))
}

#' Match Labels
#'
#' This function finds new labels for \code{b} that best match the labels
#'  of \code{a}. It can be used to give different clustering results a common
#'  vocabulary so that they can be compared with one another.
#'
#' @param a,b Factors.
#' @param strict A boolean. Toggles whether the labels in \code{b} should
#'  have a 1-to-1 mapping to the labels in \code{a}. If \code{FALSE}, the
#'  multiple labels in \code{b} can map to one label in \code{a}.
#' @param verbose A boolean. Toggles whether to print intermediate results.
#' @return A vector of new labels for \code{b}.
#' @export
match_labels <- function(a, b, strict = FALSE, verbose = FALSE){

  if(strict){

    key <- s_relAcc(a, b, return_summary = TRUE)

  }else{

    overlap <- table(a, b)
    key <-
      data.frame("label1" = apply(overlap, 2, function(x) names(rmax(x))),
                 "label2" = colnames(overlap))
  }

  # Assign 'b' a new label given best match with 'a'
  res <- vector("character", length(b))
  for(row in 1:nrow(key)){
    old_label <- as.character(key$label2[row]) # coerce from factor
    new_label <- as.character(key$label1[row])
    res[as.character(b) == old_label] <- new_label
  }

  # Assign any missing 'b' labels the label from 'a'
  res[res == ""] <- a[res == ""]

  if(verbose){
    print("Quality control check:")
    print(table(a, res))
  }

  return(res)
}
