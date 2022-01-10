#' Circularize a Difference Measure
#'
#' The difference measures included in this package compare two
#'  factor-class vectors of the same size. The measures do not consider
#'  any different alignments of the vectors. With \code{circularize}, a
#'  difference function is made to consider multiple alignments.
#'
#' @param FUN The function to circularize.
#' @param max_shift An integer. The number of alignments to try.
#'  When \code{max_shift = 1}, only the default alignment is used.
#' @param return_min A logical. Toggles whether the function
#'  should return all difference measures, or the smallest.
#' @return A function. An instance of \code{FUN} that now
#'  considers multiple alignments.
#' @export
circularize <- function(FUN, max_shift, return_min = TRUE){

  function(a, b, ...){

    args <- as.list(substitute(list(...)))[-1]

    # Apply circular shift to candidate sequence
    shuffle_index <- lapply(1:max_shift, function(k){
      if(k == 1){
        1:length(b)
      }else{
        c(
          k:length(b),
          1:(k-1)
        )
      }
    })

    shuffle_values <- sapply(shuffle_index, function(i){
      do.call(FUN,
              append(
                list(
                  a = a,
                  b = b[i]
                ),
                args)
      )
    })

    if(return_min){
      return(min(shuffle_values))
    }else{
      return(shuffle_values)
    }
  }
}
