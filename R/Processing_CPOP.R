library(CCA)

<<<<<<< HEAD
#' calculateCCA
#' Returns a matrix of canonical correlation coefficients for a list of data.frames.
#'
#' @param x_list a list of data.frames with identical features as columns.
#'
#' @return
#' @export
#'
#' @examples
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' x3 = cpop_data_binary$x3
#'
#' x_list <- list(x1,x2,x3)
#' calculateCCA(x_list)

calculateCCA <- function(x_list){
  if(ncol(x_list[[1]]) > 1000){
    warning("The number of features is > 1000, CCA will take a while to calculate.")
  }
  cor_mat <- sapply(seq_along(x_list), function(i){
    sapply(seq_along(x_list), function(j){
      CCA::cc(t(x_list[[i]]), t(x_list[[j]]))$cor[1]
    })
  })
  return(cor_mat)
=======
# Calculate the cononical correlation for a list of datasets
CCA_Calculate <- function(dataset_list) {
    # Create a list to store the canonical correlation
    cca_list <- list()

    # Loop through the list of datasets
    for (i in seq_along(dataset_list)) {
        # Loop through the list of datasets again
        for (j in seq_along(dataset_list)) {
            # If the datasets are different
            if (i != j) {
                # Calculate the canonical correlation
                cca <- CCA::cc(dataset_list[[i]], dataset_list[[j]])

                # Save the canonical correlation
                cca_list[[paste(names(dataset_list)[i], names(dataset_list)[j], sep = "_")]] <-
                    cca$canon.cor
            }
        }
    }

    # Return the list of canonical correlations
    return(cca_list)
>>>>>>> efcad19cdb7f3e9adea5cf268a376c008929d890
}
