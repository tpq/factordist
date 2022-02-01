#' Simulate Factor Data
#'
#' @param nrows An integer. The number of rows (as samples).
#' @param ncols An integer. The number of columns (as variables).
#' @param nletters An integer. The number of letters.
#' @return A data.frame where each column is a factor.
#' @export
simulate_factor_data <- function(nrows, ncols, nletters = 26){

  variable_list <- lapply(1:ncols, function(i){
    obs <- letters[sample(1:nletters, size = nrows, replace = TRUE)]
  })
  sim_data <- data.frame(variable_list)
  colnames(sim_data) <- paste0("V", 1:ncols)
  as_factor_frame(sim_data)
}
