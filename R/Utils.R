# Calculate the fold changes for each dataset.
#' lfc_calculate
#' @noRd
lfc_calculate <- function(df, y) {
    outcome <- levels(y)
    return(colSums(df[which(y == outcome[2]), ]) - colSums(df[which(y == outcome[1]), ]))
}

# Loop through possible exponents of weights_lasso
#' selectExponent
#' @noRd
selectExponent <- function(
    lasso_x,
    lasso_y,
    sample.weights = NULL,
    moderated_test,
    nCores = nCores
) {

    parallel <- FALSE
    # register parallel cluster
    if (nCores > 1) {
        parallel <- TRUE
        doParallel::registerDoParallel(nCores)
    }

    # Assign possible exponents of weights_lasso
    exponents <- 2^(seq(-3, 3, by = 0.5))
    resub_error <- list()
    for (exponent in exponents) {
        weights_lasso <- 1 / (moderated_test)^(exponent)
        if (!is.null(sample.weights)) {
            model <- glmnet::cv.glmnet(
                x = as.matrix(lasso_x),
                y = lasso_y,
                family = "binomial",
                weights = sample.weights,
                penalty.factor = weights_lasso,
                alpha = 1,
                parallel = parallel
            )
        } else if (is.null(sample.weights)) {
            model <- glmnet::cv.glmnet(
                x = as.matrix(lasso_x),
                y = lasso_y,
                family = "binomial",
                penalty.factor = weights_lasso,
                alpha = 1,
                parallel = parallel
            )
        }
        resub_error[[exponent]] <- model$cvm
    }

    # Which exponent gives the lowest resubstitution error?
    best_exponent <- exponents[which.min(resub_error)]
    return(best_exponent)
}

#' @title str_split_n
#' @importFrom stringr str_split
#' @noRd
str_split_n <- function(string, pattern, n) {
    out <- stringr::str_split(string, pattern, simplify = TRUE)
    apply(out, 1, `[`, i = n)
}

#' @title colCoxTests_combine
#' @importFrom dplyr select
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom purrr reduce
#' @importFrom directPA geneStats
#' @noRd
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
    comb.pvalues <- apply(ZScore_output, 1, directPA::geneStats)
    comb.zscores <- stats::qnorm(comb.pvalues, lower.tail = FALSE)
    pvalue2sided <- 2 * stats::pnorm(-abs(comb.zscores))
    sig.genes <- names(pvalue2sided[pvalue2sided != 0] |> sort())[1:nFeatures]
    return(sig.genes)
}

#' @title extractAUC
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
#' @noRd

extractAUC <- function(roc_list){
  auc <- lapply(roc_list, function(x){
    x$auc
  })
  auc <- auc |>
    data.frame() |>
    reshape2::melt() |>
        dplyr::mutate(
            label_long = paste0(variable, " , AUC = ", paste(round(value, 2))),
            label_AUC = paste0("AUC = ", paste(round(value, 2)))
        )
  return(auc)
  auc$variable <- names(roc_list)
}

#' @title The expit function
#' @param x numeric
#' @rdname logit-expit
#' @export
#' @examples
#' curve(expit, from = -5, to = 5)
#' @return The expit of x
expit = function(x){
  return(1/(1+exp(-x)))
}
