#' Coerce Data Frame Into "Factor Frame"
#'
#' This backend function coerces any matrix or data.frame into a
#'  new data.frame where each column is a factor.
#'
#' @param data Any matrix or data.frame.
#' @return A data.frame where each column is a factor.
#' @export
as_factor_frame <- function(data){

  if(inherits(data, "matrix")) data <- data.frame(data, stringsAsFactors = FALSE)
  if(!inherits(data, "data.frame")) stop("Input must be a data.frame.")
  if(any(is.na(data))) message("Alert: NAs detected. NA handling depends on metric.")
  isfact <- sapply(data, is.factor)
  issamelevel <- sapply(data, function(x) identical(levels(x), levels(data[,1])))

  # Coerce input into factors that all have the same levels
  if(any(!isfact) | any(!issamelevel)){
    message("Alert: Coercing all factors to have same levels.")
    all_levels <- sort(unique(unlist(data)))
    for(i in 1:ncol(data)){
      data[,i] <- factor(data[,i], levels = all_levels)
    }
  }

  data
}

#' Print Progress Bar
#'
#' This backend function prints a progress bar.
#'
#' @param i The current iteration.
#' @param k Total iterations.
#' @param numTicks The result of \code{progress}.
#' @return The next \code{numTicks} argument.
#' @export
progress <- function(i, k, numTicks){

  if(i == 1) numTicks <- 0

  if(numTicks == 0) cat("|-")

  while(i > numTicks*(k/40)){

    cat("-")
    if(numTicks == 10) cat("(25%)")
    if(numTicks == 20) cat("(50%)")
    if(numTicks == 30) cat("(75%)")
    numTicks <- numTicks + 1
  }

  if(i == k) cat("-|\n")

  numTicks
}

#' A Simple(r) \code{sample} Function
#'
#' See \code{?sample}.
#'
#' @param x A vector.
#' @param ... Arguments to \code{sample}.
#' @return A vector.
#' @export
resample <- function(x, ...){

  x[sample.int(length(x), ...)]
}

#' Get Index of Maximum Value
#'
#' @param x A vector.
#' @return The index of the maximum of the vector. If there are
#'  multiple maximums, returns an index randomly.
#' @export
which_max <- function(x){

  resample(which(x == max(x)))[1]
}

#' Get Index of Minimum Value
#'
#' @param x A vector.
#' @return The index of the minimum of the vector. If there are
#'  multiple minimum, returns an index randomly.
#' @export
which_min <- function(x){

  resample(which(x == min(x)))[1]
}

#' Get Mode
#'
#' @param x A vector.
#' @return The mode of the vector. If there are multiple modes,
#'  returns a mode randomly.
#' @export
mode <- function(x){

  counts <- table(x)
  names(which_max(counts))
}

#' Coerce \code{...} to List
#'
#' @param ... Arguments.
#' @return A list of arguments.
#' @export
args_as_list <- function(...){

  as.list(eval(list(...)))
}

#' A Simple(r) \code{do.call} Function
#'
#' @param FUN A function to call.
#' @param ... Arguments.
#' @param .prior_arg_list More arguments. Used to pass an argument
#'  list collected from \code{\link{args_as_list}}. Unlike \code{...},
#'  this can be used to pass empty arguments, by setting
#'  \code{.prior_arg_list = list()}.
#' @return The result of \code{FUN}.
#' @export
do_call <- function(FUN, ..., .prior_arg_list = list()){

  args <- args_as_list(...)
  args <- append(args, .prior_arg_list)
  do.call(FUN, args)
}
