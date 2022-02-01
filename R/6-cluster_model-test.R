#' @rdname cluster_model
#' @export
setMethod(
  "predict",
  signature(object = "cluster_model"),
  function(object, newdata, output_list = FALSE){

    # Transpose because x[,i] is faster than x[i,]
    newdata <- t(newdata)
    origdata <- t(object@x)

    if(!identical(nrow(newdata), nrow(origdata))){
      stop("Feature space of new data does not match original data.")
    }

    # Apply FUN
    diff_mat <- matrix(0, ncol(newdata), ncol(origdata))
    for(i_new in 1:ncol(newdata)){

      numTicks <- progress(i_new, ncol(newdata), numTicks)

      for(i_orig in 1:ncol(origdata)){

        v1 <- do_call(object@FUN, origdata[,i_orig], newdata[,i_new],
                      .prior_arg_list = object@args)

        # If FUN is asymmetrical, take smallest of two runs
        if(object@asym){
          v2 <- do_call(object@FUN, newdata[,i_new], origdata[,i_orig],
                        .prior_arg_list = object@args)
          diff_mat[i_new, i_orig] <- min(v1, v2)
        }else{
          diff_mat[i_new, i_orig] <- v1
        }
      }
    }

    # Check how many nearest neighbors each new datum has
    nearest_neighbors <-
      lapply(1:nrow(diff_mat), function(i){
        which(diff_mat[i,] == min(diff_mat[i,]))
      })

    # Give NA when there is no nearest neighbor (i.e., equally distance to all neighbors)
    index_no_neighbors <- sapply(nearest_neighbors, length) == ncol(diff_mat)
    if(any(index_no_neighbors)){
      message("Some samples have no nearest neighbors. Assigning NA label.")
      nearest_neighbors[index_no_neighbors] <-
        lapply(nearest_neighbors[index_no_neighbors], function(x) NA)
    }

    # Sample neighbor when datum has multiple neighbors
    index_multiple_neighbors <- sapply(nearest_neighbors, length) > 1
    if(any(index_multiple_neighbors)){
      message("Some samples have multiple nearest neighbors. Choosing randomly.")
      nearest_neighbors[index_multiple_neighbors] <-
        lapply(nearest_neighbors[index_multiple_neighbors], function(x) sample(x)[1])
    }

    # Assign new labels based on the nearest neighbor
    nearest_neighbor <- unlist(nearest_neighbors)
    new_k <- object@y[nearest_neighbor]

    # Name results
    colnames(diff_mat) <- colnames(origdata)
    rownames(diff_mat) <- colnames(newdata)
    names(new_k) <- colnames(newdata)

    if(output_list){
      return(
        list(
          "new_differences" = diff_mat,
          "new_y" = new_k
        )
      )
    }else{
      return(new_k)
    }
  }
)
