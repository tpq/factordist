#' Build Model of Cluster Assignmentss
#'
#' The \code{cluster_model} class and methods are intended to make
#'  it easier to generalize clustering results to new data.
#'  Its design mirrors methods used for supervised learning.
#'  First, an initial function builds a model that relates the input
#'  data \code{x} to the labels \code{y}. Second, another function deploys
#'  the model to \code{newdata}, for which labels are unknown or hidden.
#'  Unlike supervised learning, the labels for the new data get
#'  assigned procedurally using a nearest neighbor algorithm. Put
#'  simply, each new data point in \code{newdata} is given the label of
#'  the the point in \code{x} to which it is closest. Ties are broken
#'  randomly. The distance used is given by \code{FUN}.
#'
#' @examples
#' library(factordist)
#' x <- simulate_factor_data(30, 6)
#' y <- sample(c("A", "B"), replace = TRUE, size = 30)
#' newdata <- simulate_factor_data(10, 6)
#' m <- cluster_model(x, y, FUN = d_jaccard)
#' predict(m, newdata)
#'
#' @slot x Stores the data that has been clustered.
#' @slot y Stores the cluster group assignments.
#' @slot FUN Stores a difference function.
#' @slot asym Stores whether the function is asymmetrical.
#' @slot args Stores arguments to the difference function.
#'
#' @param x The data that has been clustered.
#'  For S3 methods, \code{x} refers to a \code{cluster_model} object.
#' @param y The cluster group assignments.
#' @param FUN The difference function to use. The first two
#'  arguments for this function should correspond to vectors
#'  of the two samples to compare. Additional arguments
#'  can be provided via \code{...}.
#' @param asym A boolean. Toggles whether the \code{FUN} is asymmetrical,
#'  i.e., dist(a, b) != dist(b, a). When TRUE, the \code{predict}
#'  method will choose each neighbor based on the smaller of the
#'  two distances.
#' @param ... Arguments to the difference function.
#' @param object A \code{cluster_model} object.
#' @param newdata New data for which to assign cluster groups.
#' @param output_list A boolean. Toggles whether the \code{predict}
#'  method should output a list of the distances used to select
#'  the nearest neighbor.
#'
#' @name cluster_model
NULL

#' @rdname cluster_model
#' @export
setClass(
  "cluster_model",
  slots = c(
    x = "matrix",
    y = "character",
    FUN = "function",
    asym = "logical",
    args = "list"
  )
)

#' @rdname cluster_model
#' @export
setMethod(
  "print",
  signature(x = "cluster_model"),
  function(x){

    cat("Dimension of the data clustered:")
    cat("\n> ")
    cat(dim(x@x))
    cat("\n")
    cat("Number of clusters:")
    cat("\n> ")
    cat(length(unique(x@y)))
    cat("\n")
    cat("Use predict() to deploy.")
    cat("\n")
  }
)

#' @rdname cluster_model
#' @export
setMethod(
  "show",
  "cluster_model",
  function(object) print(object)
)

#' @rdname cluster_model
#' @export
cluster_model <- function(x, y, FUN, asym = FALSE, ...){

  args <- as.list(substitute(list(...)))[-1]

  if(any(class(x) == "data.frame")){
    x <- as.matrix(x)
  }else if(any(class(x) == "matrix")){
    x <- x
  }else{
    stop("Class for x not recognized.")
  }

  if(any(class(y) %in% c("matrix", "data.frame"))){
    y <- y[,1]
  }

  if(any(class(y) %in% c("factor", "numeric", "integer"))){
    message("Alert: Coercing label input to string.")
    y <- as.character(y)
  }else if(any(class(y) == "character")){
    y <- y
  }else{
    stop("Class for y not recognized.")
  }

  if(nrow(x) != length(y)){
    stop("Rows in x should correspond to y.")
  }

  model <- new("cluster_model")
  model@x <- as.matrix(x)
  model@y <- y
  model@FUN <- FUN
  model@args <- args
  model@asym <- asym

  return(model)
}

#' @rdname cluster_model
#' @export
setMethod(
  "predict",
  signature(object = "cluster_model"),
  function(object, newdata, output_list = FALSE){

    if(any(class(newdata) == "data.frame")){
      newdata <- as.matrix(newdata)
    }else if(any(class(newdata) == "matrix")){
      newdata <- newdata
    }else{
      stop("Class for newdata not recognized.")
    }

    origdata <- object@x
    if(!identical(ncol(newdata), ncol(origdata))){
      stop("Feature space of new data does not match original data.")
    }

    # Apply FUN
    diff_mat <- matrix(0, nrow(newdata), nrow(origdata))
    for(i_new in 1:nrow(newdata)){
      for(i_orig in 1:nrow(origdata)){

        v1 <- do.call(object@FUN,
                      append(list(origdata[i_orig,],
                                  newdata[i_new,]), object@args))

        # If FUN is asymmetrical, take smallest of two runs
        if(object@asym){
          v2 <- do.call(object@FUN,
                        append(list(newdata[i_new,],
                                    origdata[i_orig,]), object@args))
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
    multiple_neighbors <- sapply(nearest_neighbors, length) == ncol(diff_mat)
    if(any(multiple_neighbors)){
      message("Some samples have no nearest neighbors. Assigning NA label.")
      nearest_neighbors[multiple_neighbors] <-
        lapply(nearest_neighbors[multiple_neighbors], function(x) NA)
    }

    # Sample neighbor when datum has multiple neighbors
    multiple_neighbors <- sapply(nearest_neighbors, length) > 1
    if(any(multiple_neighbors)){
      message("Some samples have multiple nearest neighbors. Choosing randomly.")
      nearest_neighbors[multiple_neighbors] <-
        lapply(nearest_neighbors[multiple_neighbors], function(x) sample(x)[1])
    }

    # Assign new labels based on the nearest neighbor
    nearest_neighbor <- unlist(nearest_neighbors)
    new_k <- object@y[nearest_neighbor]

    # Name results
    colnames(diff_mat) <- rownames(object@x)
    rownames(diff_mat) <- rownames(newdata)
    names(new_k) <- rownames(newdata)

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
