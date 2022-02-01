#' @import methods
NULL

#' Calculate Distance Between Factors
#'
#' @param data A data.frame of factors. Distances and differences
#'  are calculated between columns of the input. If data does not contain
#'  factors, factors are introduced by \code{\link{as_factor_frame}}.
#' @param metric A string. The name of the difference metric.
#' @param as_dist A boolean. Toggles whether to return the
#'  difference matrix as a Base R \code{dist} object.
#'
#' @return A difference matrix.
#'
#' @details
#'
#' A metric can be categorical, ranked, absolute, or relative.
#'  Categorical metrics treat input as unranked characters.
#'  Ranked metrics treat input as ranked factors.
#'  Absolute metrics compare factor labels in one input directly
#'   with factor labels in the other input. For example, a=c(1,1,2,2) is
#'   different from b=c(2,2,1,1) because 1 is not the same as 2.
#'  Relative metrics compare groups of factor labels in one input
#'   with groups of factor labels in the other input. For example,
#'   a=c(1,1,2,2) is the same as b=c(2,2,1,1) because the first and
#'   second index, as well as the third and fourth index, always
#'   have the same label.
#'
#' @examples
#' library(factordist)
#' x <- simulate_factor_data(30, 6)
#' dis <- factordist(x, metric = "adjRand")
#'
#' @export
factordist <- function(data, metric = c("jaccard", "adjRand"), as_dist = TRUE){

  data <- as_factor_frame(data)

  metric <- metric[1]
  if(metric == "jaccard"){
    dif <- papply(data, MARGIN = 2, FUN = d_jaccard, lower_only = FALSE)
  }else if(metric == "adjRand"){
    dif <- papply(data, MARGIN = 2, FUN = d_adjRand, lower_only = FALSE)
  }else{
    stop("Metric not recognized. See ?factordist.")
  }

  if(as_dist){
    return(stats::as.dist(dif))
  }else{
    return(dif)
  }
}
