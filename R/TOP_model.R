#' @title TOP_model
#' @description The main function of the TOP package. This function returns a glmnet model .
#' @param x_list a list of data frames, each containing the data for a single batch or dataset. Columns should be features and rows should be observations.
#' @param y_list a list of factors, each containing the labels for a single batch or dataset. The length of this list should be the same as the length of x_list.
#' @param covariates a list of data frames with the covariates that should be included in the model, Default: NULL
#' @param dataset_weights a list of data frames that refer to any grouping structure in the batches, Default: NULL
#' @param sample_weights Should each batch we weighted equally? This is important in unequal sample sizes, Default: FALSE
#' @param optimiseExponent Should the exponent used to modufy the lasso weights be optimised using resubstitution?, Default: FALSE
#' @param nCores A numeric specifying the number of cores used if the user wants to use parallelisation, Default: 1
#' @return Returns a list with the following elements: models, which is a glmnet object and features, which is a list of the features used in each model.
#' @examples
#' data(cpop_data_binary, package = "CPOP")
#'
#' x1 <- cpop_data_binary$x1
#' x2 <- cpop_data_binary$x2
#' x3 <- cpop_data_binary$x3
#' y1 <- cpop_data_binary$y1
#' y2 <- cpop_data_binary$y2
#' y3 <- cpop_data_binary$y3
#'
#' set.seed(23)
#' x_list <- list(x1, x2)
#' y_list <- list(factor(y1), factor(y2))
#'
#' model <- TOP_model(x_list, y_list)
#'
#' @rdname TOP_model
#' @export
#' @importFrom dplyr mutate group_by summarise
#' @importFrom tibble rownames_to_column
#' @importFrom tibble enframe
#' @importFrom Hmisc wtd.var
#' @importFrom glmnet makeX cv.glmnet
#' @importFrom doParallel registerDoParallel
TOP_model <- function(
    x_list, y_list, covariates = NULL, dataset_weights = NULL,
    sample_weights = FALSE, optimiseExponent = FALSE, nCores = 1
) {
    # Catching some errors.
    # y must be a factor or else it will break.
    if (sum(!unlist(lapply(y_list, is.factor))) > 0) {
        factor_rank <- which(!unlist(lapply(y_list, is.factor)) != FALSE)
        stop("The outcome in positiion ", factor_rank, " is not a factor")
    }

    parallel <- FALSE
    # register parallel cluster
    if (nCores > 1) {
        parallel <- TRUE
        doParallel::registerDoParallel(nCores)
    }

    # Convert counts to ratios
    message("Calculating Pairwise Ratios of Features")
    x_list <- lapply(x_list, as.matrix)
    z_list <- lapply(x_list, pairwise_col_diff)

    # Calculating the log fold change of each gene between conditions
    lfc <- list()
    message("Calculating Fold Changes of Pairwise Ratios")
    for (i in seq_along(z_list)) {
        lfc[[i]] <- lfc_calculate(z_list[[i]], y_list[[i]])
    }

    ## If there is some weights supplied
    if (!is.null(dataset_weights)) {
        # Sample weights
        message("Calculating Weights for each Dataset")
        sample.weights <- unlist(dataset_weights) |>
            tibble::enframe() |>
            dplyr::mutate(Organ = as.character(value)) |>
            dplyr::group_by(Organ) |>
            dplyr::summarise(n = dplyr::n()) |>
            dplyr::mutate(freq = n / sum(n))
        un_weights <- unlist(dataset_weights) |>
            tibble::enframe() |>
            dplyr::mutate(
                Organ = as.character(value),
                weight = 1
            )
        for (i in seq_len(nrow(un_weights))) {
            idx <- which(un_weights$Organ[i] == sample.weights$Organ)
            un_weights$weight[i] <- sample.weights$freq[idx]
        }
        sample.weights <- un_weights$weight
    }

    # We want to account for datasets that do not have equal sample numbers
    ## If there are sample weights.
    if (sample_weights == TRUE) {
        message("Modifing Fold Change of Ratios based on sample_weights")
        lfc <- do.call("cbind", lfc)

        freq_samples <- sapply(x_list, dim)[1, ] |>
            tibble::enframe() |>
            dplyr::mutate(freq = value/sum(value)) |>
            dplyr::mutate(inv_freq = 1 / freq)

        aggregate_lfc <- abs(
            apply(lfc, 1, function(x) stats::weighted.mean(x, freq_samples$inv_freq))
        )
        variance_lfc <- sqrt(
            apply(lfc, 1, function(x) Hmisc::wtd.var(x, freq_samples$inv_freq))
        )
    }
    ## If there are none supplied.
    else if (sample_weights == FALSE) {
        lfc <- do.call("cbind", lfc)
        aggregate_lfc <- abs(apply(lfc, 1, mean))
        variance_lfc <- apply(lfc, 1, sd)
    }

    fudge_vector <- variance_lfc[variance_lfc != 0]
    fudge <- stats::quantile(fudge_vector, 0.05, na.rm = TRUE)

    # Take a moderated test statistic
    message("Calculating Final Weights")
    moderated_test <- aggregate_lfc / (variance_lfc + fudge)

    # Preparring data for lasso model.
    lasso_x <- do.call("rbind", z_list)
    lasso_y <- factor(unlist(y_list))

    # Adding covariates to the model.
    if (!is.null(covariates)) {
        message("Fitting covariates into the model")
        covariates <- do.call("rbind", covariates)
        covariates <- covariates |>
            data.frame()

        # Adding covariates to the final matrix.
        lasso_x <- cbind(lasso_x, covariates)
        lasso_x <- glmnet::makeX(methods::as(lasso_x, "data.frame"))

        # Altering the wights of the final lasso model
        covariate_weights <- rep(
            Inf, ncol(glmnet::makeX(methods::as(covariates, "data.frame")))
        )
        moderated_test <- append(moderated_test, covariate_weights)
        names(moderated_test) <- colnames(lasso_x)
    }

    # Using selectExponent to determine best exponent
    if (optimiseExponent == TRUE) {
        message("Determining Best Exponent")
        exponent <- selectExponent(
            lasso_x, lasso_y, moderated_test, sample.weights = sample.weights
        )
        weights_lasso <- 1 / (moderated_test)^(exponent)
        message(paste("The best exponent was: ", exponent))
    } else if (optimiseExponent == FALSE) {
        weights_lasso <- 1 / (moderated_test)^(1 / 2)
    }

    # Lasso model for all datasets with updated weights
    if (!is.null(dataset_weights)) {
        message("Fitting final lasso model")
        model <- glmnet::cv.glmnet(
            x = as.matrix(lasso_x),
            y = lasso_y,
            family = "binomial",
            weights = sample.weights,
            penalty.factor = weights_lasso,
            alpha = 1,
            parallel = parallel
        )
    } else if (is.null(dataset_weights)) {
        message("Fitting final lasso model")
        model <- glmnet::cv.glmnet(
            x = as.matrix(lasso_x),
            y = lasso_y,
            family = "binomial",
            penalty.factor = weights_lasso,
            alpha = 1,
            parallel = parallel
        )
    }

    result <- list(
        models = model,
        feature = moderated_test
    )
    return(result)
}


#' @title Prectict using the Trasferable Omics Prediction model.
#' @description A prediction function for the Trasferable Omics Prediction model.
#' @param TOP_model The output from the TOP_model function.
#' @param newx A matrix of the new data to be predicted. The columns should be features and the rows should be samples.
#' @param covariates A data frame of the same covariates that were used in the TOP model, Default: NULL
#' @param s Lambda value for the lasso model, Default: 'lambda.min'
#' @return A vector of predictions for the new data.
#' @examples
#' data(cpop_data_binary, package = "CPOP")
#'
#' x1 <- cpop_data_binary$x1
#' x2 <- cpop_data_binary$x2
#' x3 <- cpop_data_binary$x3
#' y1 <- cpop_data_binary$y1
#' y2 <- cpop_data_binary$y2
#' y3 <- cpop_data_binary$y3
#'
#' set.seed(23)
#' x_list <- list(x1, x2)
#' y_list <- list(factor(y1), factor(y2))
#'
#' model <- TOP_model(x_list, y_list)
#' predictions <- predict_TOP(model$models, newx = x3)
#' @rdname predict_TOP
#' @export
#' @importFrom glmnet makeX
predict_TOP <- function(TOP_model, newx, covariates = NULL, s = "lambda.min") {
    # Determine z for the new x
    newz <- pairwise_col_diff(newx)
    if (!is.null(covariates)) {
        w3 <- glmnet::makeX(cbind(newz, covariates))
        result_response <- stats::predict(
            object = TOP_model, newx = w3, s = s,
            type = "response"
        )
    } else {
        result_response <- stats::predict(
            object = TOP_model, newx = newz, s = s,
            type = "response"
        )
    }
    return(result_response)
}
