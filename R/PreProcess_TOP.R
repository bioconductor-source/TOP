#' @title filterFeatures
#' @description A function that implements feature selection, using limma, from a list of data frames with corresponding labels.
#' @param x_list A list of data frames, with columns corresponding to features and rows corresponding to observations.
#' @param y_list A list of factor labels.
#' @param contrast A character vector describing which order of levels to contrast in y_list ("disease - control"), Default: NULL
#' @param nFeatures Number of features to return, Default: 50
#' @param combinationMethod Which p-value combination method to use, Default: 'OSP' Options are 'Stouffer', 'OSP', 'Fisher', 'maxP'.
#' @return A vector of feature names.
#' @details contrast must be a character vector of length 1. If contrast is NULL, the first level of the first factor in y_list will be used as the reference level.
#' @examples
#' data(TOP_data_binary, package = "TOP")
#' x1 = TOP_data_binary$x1
#' x2 = TOP_data_binary$x2
#' x3 = TOP_data_binary$x3
#'
#' x_list <- list(x1,x2,x3)
#' y_list <- list(TOP_data_binary$y1, TOP_data_binary$y2, TOP_data_binary$y3)
#' y_list <- y_list <- lapply(y_list, function(x){x <- factor(x, levels = c("1", "0"), labels = c("Yes", "No"))})
#'
#' filterFeatures(x_list, y_list, contrast = "Yes - No", nFeatures = 10, combinationMethod = "OSP")
#'
#' @rdname filterFeatures
#' @export
#' @importFrom limma lmFit makeContrasts contrasts.fit eBayes topTable
#' @importFrom dplyr select
#' @importFrom tibble remove_rownames column_to_rownames
#' @importFrom directPA geneStats
filterFeatures <- function(x_list, y_list, contrast = NULL, nFeatures = 50, combinationMethod = "OSP") {
    if (!combinationMethod %in% c("Stouffer", "OSP", "Fisher", "maxP")) {
        stop(print("Available methods are Stouffer, OSP, Fisher, or maxP"))
    }

    # Transpose x_list
    x_list <- lapply(x_list, t)

    tT <- list()
    for (i in seq_along(x_list)) {
        # Assign levels to y_list if contrasts are not given.
        if (is.null(contrast)) {
            level <- levels(y_list[[i]])
            contrast <- level[2] - level[1]
        }

        # Make sure y_list is a factor.
        y_list[[i]] <- factor(y_list[[i]])
        # Make sure x_list is a matrix.
        x_list[[i]] <- as.matrix(x_list[[i]])

        # Set up a design matrix according to y_list
        des <- stats::model.matrix(~ 0 + y_list[[i]])
        # Make the names of the columns in the design matrix the same as the levels of y_list
        colnames(des) <- levels(y_list[[i]])

        # Run limma
        fit <- limma::lmFit(x_list[[i]], design = des)
        CM <- limma::makeContrasts(contrasts = contrast, levels = des)
        fit2 <- limma::contrasts.fit(fit, CM)
        efit <- limma::eBayes(fit2, robust = TRUE)
        tT[[i]] <- limma::topTable(efit, coef = contrast, n = Inf) |>
            dplyr::select(t) |>
            data.frame()

        # Add a column with the gene names
        tT[[i]]$gene <- rownames(tT[[i]])
    }
    # merge a the tT list into a single data frame by gene
    suppressWarnings(
        tT <- Reduce(function(x, y) merge(x, y, by = "gene", all = TRUE), tT)
    )

    # Keep unique genes
    tT <- tT[!duplicated(tT$gene), ]
    tT <- tT |>
        tibble::remove_rownames() |>
        tibble::column_to_rownames(var = "gene")

    # Run directPA
    Z.Scores.All <- apply(tT, 2, function(x) {
        stats::qnorm(rank(x) / (nrow(tT) + 1))
    })
    utils::data(Pathways, package = "directPA")
    gene.pvalues <- apply(Z.Scores.All, 1, function(x) {
        directPA::geneStats(x, method = combinationMethod)
    })
    gene.zscores <- stats::qnorm(gene.pvalues, lower.tail = FALSE)
    pvalue2sided <- 2 * stats::pnorm(-abs(gene.zscores))
    sig.genes <- names(pvalue2sided[pvalue2sided != 0] |> sort())[1:nFeatures]

    return(sig.genes)
}
