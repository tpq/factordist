#' Make Progress Bar
#'
#' @param i The current iteration.
#' @param k Total iterations.
#' @param numTicks The result of \code{progress}.
#' @return The next \code{numTicks} argument.
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

  return(numTicks)
}
