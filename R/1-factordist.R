#' @import methods
NULL

#' Calculate Distance Between Factors
#'
#' @param data A data.frame. Distances and differences
#'  are calculated between columns of the input.
#' @param metric A string. The distance or difference metric.
#' @param as_dist A boolean. Toggles whether to return the
#'  result matrix as dist object.
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
factordist <- function(data, metric = c("adjRand", "jaccard"), as_dist = TRUE){

  if(class(data) == "matrix") data <- data.frame(data, stringsAsFactors = FALSE)
  if(class(data) != "data.frame") stop("Input must be a data.frame.")
  if(any(is.na(data))) message("Alert: NAs detected. NA handling depends on metric.")
  isfact <- sapply(data, is.factor)

  # Coerce input into factors that all have the same levels
  if(any(!isfact)){
    message("Alert: Coercing all non-factor input into factors.")
    all_levels <- sort(unique(unlist(data)))
    for(i in 1:ncol(data)){
      data[,i] <- factor(data[,i], levels = all_levels)
    }
  }

  issamelevel <- sapply(data, function(x) all(levels(x) == levels(data[,1])))
  if(any(!issamelevel)){
    message("Alert: Coercing all factors to have same levels.")
    all_levels <- sort(unique(unlist(data)))
    for(i in 1:ncol(data)){
      data[,i] <- factor(data[,i], levels = all_levels)
    }
  }

  metric <- metric[1]
  if(metric == "adjRand"){
    dif <- papply(data, MARGIN = 2, FUN = d_adjRand, lower_only = FALSE)
  }else if(metric == "jaccard"){
    dif <- papply(data, MARGIN = 2, FUN = d_jaccard, lower_only = FALSE)
  }else{
    stop("Metric not recognized. See ?factordist.")
  }

  if(as_dist){
    return(stats::as.dist(dif))
  }else{
    return(dif)
  }
}
