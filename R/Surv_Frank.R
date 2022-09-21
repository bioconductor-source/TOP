library(survival)
library(ClassifyR)
library(dplyr)

Surv_Frank <- function(x_list, y_list) {
  
  # create a loop to run through all datasets
  output <- list()
  for(i in seq_along(x_list)) {
    # Perform colCoxTests on each dataset
    output[[i]] <- ClassifyR::colCoxTests(as.matrix(x_list[[i]]), y_list[[i]], option = "fast")
  }
  
  sig.genes <- sort(colCoxTests_combine(output))
  
  pairwise_coefficients <- list()
  for(i in seq_along(x_list)) {
    # subset x_list with the rownames of output
    x_subset <- x_list[[i]][,sig.genes]
    
    # Calculate the pairwise_col_diff of z_list_subset
    z_subset <- CPOP::pairwise_col_diff(x_subset)
    
    # Run colCoxTests on z_subset
    pairwise_coefficients[[i]] <- ClassifyR::colCoxTests(z_subset, y_list[[i]], option = "fast") %>%
      dplyr::select(coef) %>%
      data.frame() %>%
      tibble::rownames_to_column(var = "Gene")
  }
  
  # Merging pairwise_coefficients
  coefficients <- pairwise_coefficients %>%
    purrr::reduce(left_join, by = "Gene") %>%
    tibble::column_to_rownames(var = "Gene")

  # Calculate the average & sd of the coefficients across Datasets
  mean_coefficients <- abs(rowMeans(coefficients))
  sd_coefficients <- apply(coefficients, 1, sd)
  fudge <- quantile(sd_coefficients[sd_coefficients != 0], 0.05, na.rm = TRUE)

  # Calculate the final weights
  weights <- mean_coefficients / (sd_coefficients + fudge)
  final_weights <- 1/(weights)^(1/2)
  
  # Extract the pairwise ratios for significant genes
  x_list <- lapply(x_list, function(x) x[,sig.genes])
  z_list <- lapply(x_list, CPOP::pairwise_col_diff)
  z_list <- do.call("rbind", z_list)
  
  # Cleaning the outcome variable
  survival_y <- do.call("rbind", y_list)
  Surv_y <- Surv(time = survival_y[,1], event = survival_y[,2], type = "right")

  # Run a coxnet model with the final_weights as penalty factors
  coxnet_model <- glmnet::cv.glmnet(x = as.matrix(z_list),
                                 y = Surv_y, family = "cox", type.measure = "C",
                                 penalty.factor = final_weights)
  coxnet_model <- list(coxnet_model, sig.genes)

  return(coxnet_model)
}

# Create a prediction function
Surv_Frank_Pred <- function(coxnet_model, newx) {
  # Calculate the pairwise differences of x
  newx <- newx[,coxnet_model[[2]]]
  z <- CPOP::pairwise_col_diff(newx)
  
  # Predict the survival time
  survScores <- predict(coxnet_model[[1]], as.matrix(z), type = "response", newoffset = offset)
  
  return(survScores)
}

# Create a function to calculate the concordance index
Surv_Frank_CI <- function(coxnet_model, newx, newy) {
  # Calculate the pairwise differences of x
  newx <- newx[,coxnet_model[[2]]]
  z <- CPOP::pairwise_col_diff(newx)
  
  # Predict the survival time
  survScores <- predict(coxnet_model[[1]], as.matrix(z), type = "response", newoffset = offset)
  
  # Calculate the concordance index
  CI <- survival::concordance(Surv(newy[,1], newy[,2]) ~ survScores)
  
  return(CI)
}

colCoxTests_combine <- function(colCoxTests_list) {
  # Extract the p-values from each dataset
  cox_list <- list()
  for(i in seq_along(colCoxTests_list)) {
    cox_list[[i]] <- colCoxTests_list[[i]] %>%
      dplyr::select(coef) %>%
      data.frame() %>%
      tibble::rownames_to_column(var = "Gene")
  }
  
  # Combine the p-values from each dataset
  outputs <- cox_list %>% 
    purrr::reduce(left_join, by = "Gene") %>%
    tibble::column_to_rownames(var = "Gene")
  
  # Perform directPA
  ZScore_output <- apply(outputs, 2, function(x){qnorm(rank(x)/(nrow(outputs)+1))})
  data(Pathways)
  comb.pvalues <- apply(ZScore_output, 1, geneStats)
  comb.zscores <- qnorm(comb.pvalues, lower.tail = FALSE)
  pvalue2sided=2*pnorm(-abs(comb.zscores))
  sig.genes <- names(sort(pvalue2sided))[1:100]
  return(sig.genes)
}