# Calculate the fold changes for each dataset.
lfc_calculate <- function(df, y) {
    outcome <- levels(y)
    return(colSums(df[which(y == outcome[2]), ]) - colSums(df[which(y == outcome[1]), ])) # nolint
}

# Loop through possible exponents of weights_lasso
selectExponent <- function(lasso_x, lasso_y, sample.weights, moderated_test) {
    # Assign possible exponents of weights_lasso
    exponents <- 2^(seq(-5, 5, by = 1))
    resub_error <- list()

    for (i in seq_along(exponents)) {
        weights_lasso <- 1 / (moderated_test)^(exponents)

        # Fit the model
        model <- glmnet::cv.glmnet(
            x = as.matrix(lasso_x),
            y = lasso_y,
            family = "binomial",
            weights = sample.weights,
            penalty.factor = weights_lasso,
            alpha = 1
        )

        # Calculate resubstitution error of model
        resub_error[[i]] <- model$cvm[which.min(model$cvm)]
    }

    # Which exponent gives the lowest resubstitution error?
    best_exponent <- exponents[which.min(resub_error)]
    return(best_exponent)
}

# Convinient string split function.
str_split_n <- function(string, pattern, n) {
    out <- stringr::str_split(string, pattern, simplify = TRUE)
    apply(out, 1, `[`, i = n)
}
