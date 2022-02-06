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
match_labels <- function(a, b, strict = TRUE, verbose = FALSE){

  if(strict){

    key <- s_relAcc(a, b, return_summary = TRUE)

  }else{

    overlap <- table(a, b)
    key <-
      data.frame("label1" = apply(overlap, 2, function(x) names(which_max(x))),
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

#' Build Consensus Matrix
#'
#' This function runs \code{\link{match_labels}} on every column
#'  of a matrix. It can be used to give different clustering results a
#'  common vocabulary so that they can be compared with one another.
#'  In other words, this function establishes a "consensus matrix"
#'  that can be used for consensus clustering.
#'
#' @param mat A matrix or data.frame.
#' @param ref_col An integer. The column to use as a reference.
#'  If NA, the function will choose a reference automatically.
#' @param ... Arguments to \code{\link{match_labels}}.
#' @return A data.frame of matched labels.
#' @export
as_consensus_matrix <- function(mat, ref_col = NA, ...){

  mat <- as_factor_frame(mat)

  if(identical(ref_col, NA)){
    message("No reference provided. Choosing one automatically.")
    sim <- papply(mat, 2, s_relAcc)
    totalSim <- rowSums(sim)
    ref_col <- which_max(totalSim)
  }

  res <- mat
  for(col in 1:ncol(mat)){
    res[,col] <- match_labels(mat[,ref_col], mat[,col], ...)
  }

  as_factor_frame(res)
}

#' Assign Consensus Cluster Labels
#'
#' First, this function builds a "consensus matrix" to give different
#'  clustering results a common vocabulary so that they can be compared
#'  with one another. Second, this function assigns each row a final
#'  label based on which label is most common. Rows that do not have
#'  consistent labels across the "consensus matrix" are considered
#'  to be "wild cards", and given an NA label.
#'
#' @inheritParams as_consensus_matrix
#' @param wildcard_cutoff A number between 0 and 1. Sets the minimum
#'  proportion of identical labels needed for a row to be assigned a
#'  final label. For example, if \code{wildcard_cutoff = 0.5}, then a row
#'  must be assigned the label "A" in half or more of all columns to be
#'  assigned "A" as the final label. If the most common label for a row
#'  appears less frequently than the cutoff, the row is given an NA
#'  label. Use \code{wildcard_cutoff = 0} to disable this feature.
#'  When the cutoff is met, the statistical mode is used. All ties
#'  are broken randomly.
#' @return A vector of new labels.
#' @export
as_consensus_cluster <- function(mat, wildcard_cutoff = 0.5, ref_col = NA, ...){

  cons_mat <- as_consensus_matrix(mat, ref_col = ref_col, ...)

  modes <- apply(cons_mat, 1, mode)
  mode_popularity <- apply(cons_mat, 1, function(x) sum(x == mode(x))/length(x))

  new_k <- modes
  if(wildcard_cutoff > 0){
    new_k[mode_popularity < wildcard_cutoff] <- NA
  }

  new_k
}
