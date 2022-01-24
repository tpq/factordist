#' Simulate Factor Data
#'
#' @param nrows The number of rows (as samples).
#' @param ncols The number of columns (as variables).
#' @return A data.frame of factors.
#' @export
simulate_factor_data <- function(nrows, ncols){

  variable_list <- lapply(1:ncols, function(i){
    obs <- letters[sample(1:26, size = nrows, replace = TRUE)]
    obs <- factor(obs, levels = letters)
  })
  sim_data <- data.frame(variable_list)
  colnames(sim_data) <- paste0("V", 1:ncols)
  return(sim_data)
}
