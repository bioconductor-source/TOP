#' @title calculateCCA
#' @description Returns a matrix of canonical correlation coefficients for a list of data.frames.
#'
#' @param x_list a list of data.frames with identical features as columns.
#'
#' @return A matrix of canonical correlation coefficients.
#' @export
#'
#' @examples
#'
#'  data(cpop_data_binary, package = "CPOP")
#'
#'  x1 = cpop_data_binary$x1
#'  x2 = cpop_data_binary$x2
#'  x3 = cpop_data_binary$x3
#'
#'  x_list <- list(x1,x2,x3)
#'  calculateCCA(x_list)
#'
#' @importFrom CCA cc
calculateCCA <- function(x_list) {
  if (ncol(x_list[[1]]) > 1000) {
    warning("The number of features is > 1000, CCA will take a while to calculate.")
  }
  cor_mat <- sapply(seq_along(x_list), function(i){
    sapply(seq_along(x_list), function(j){
      CCA::cc(t(x_list[[i]]), t(x_list[[j]]))$cor[1]
    })
  })
  return(cor_mat)
}