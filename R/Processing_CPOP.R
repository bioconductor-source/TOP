library(CCA)

# Calculate the cononical correlation for a list of datasets
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
}
