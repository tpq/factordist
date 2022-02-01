#' @rdname cluster_model
#' @export
cluster_model <- function(x, y, FUN, asym = FALSE, ...){

  x <- as_factor_frame(x)

  if(inherits(y, c("data.frame", "matrix"))){
    if(!identical(rownames(x), rownames(y))){
      stop("Row names for input x and y do not match.")
    }
    message("Alert: Using first column of y as label.")
    y <- y[,1]
  }

  if(inherits(y, c("factor", "character", "numeric", "integer"))){
    message("Alert: Coercing input y to string.")
    y <- as.character(y)
  }else{
    stop("Class for y not recognized.")
  }

  if(nrow(x) != length(y)){
    stop("Rows in x should correspond to y.")
  }

  model <- new("cluster_model")
  model@x <- x
  model@y <- y
  model@FUN <- FUN
  model@args <- args_as_list(...)
  model@asym <- asym

  return(model)
}
