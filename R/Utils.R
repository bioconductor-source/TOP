# Calculate the fold changes for each dataset.
lfc_calculate <- function(df, y) {
    outcome <- levels(y)
    return(colSums(df[which(y == outcome[2]), ]) - colSums(df[which(y == outcome[1]), ])) # nolint
}

# Loop through possible exponents of weights_lasso
selectExponent <- function(lasso_x, lasso_y, sample.weights, moderated_test) {
    # Assign possible exponents of weights_lasso
    exponents <- 2^(seq(-3, 3, by = 0.5))
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

#' @title colCoxTests_combine
#' @importFrom dplyr select
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom purrr reduce
colCoxTests_combine <- function(colCoxTests_list, nFeatures = 50) {
    # Extract the p-values from each dataset
    cox_list <- list()
    for (i in seq_along(colCoxTests_list)) {
        cox_list[[i]] <- colCoxTests_list[[i]] |>
            dplyr::select(coef) |>
            data.frame() |>
            tibble::rownames_to_column(var = "Gene")
    }

    # Combine the p-values from each dataset
    outputs <- cox_list |>
        purrr::reduce(left_join, by = "Gene") |>
        tibble::column_to_rownames(var = "Gene")

    # Perform directPA
    ZScore_output <- apply(outputs, 2, function(x) {
        stats::qnorm(rank(x) / (nrow(outputs) + 1))
    })
    utils::data(Pathways)
    comb.pvalues <- apply(ZScore_output, 1, geneStats)
    comb.zscores <- stats::qnorm(comb.pvalues, lower.tail = FALSE)
    pvalue2sided <- 2 * stats::pnorm(-abs(comb.zscores))
    sig.genes <- names(sort(pvalue2sided))[1:nFeatures]
    return(sig.genes)
}