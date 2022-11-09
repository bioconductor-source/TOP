library(CCA)

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
}
