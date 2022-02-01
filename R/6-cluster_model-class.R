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
    x = "data.frame",
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
