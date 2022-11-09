#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x_list PARAM_DESCRIPTION
#' @param y_list PARAM_DESCRIPTION
#' @param contrast PARAM_DESCRIPTION, Default: NULL
#' @param nFeatures PARAM_DESCRIPTION, Default: 50
#' @param combinationMethod PARAM_DESCRIPTION, Default: 'OSP'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'  #EXAMPLE1
#' @rdname PreProcess_Frank
#' @export 
#' @importFrom limma lmFit makeContrasts contrasts.fit eBayes topTable
#' @importFrom dplyr select
#' @importFrom tibble remove_rownames column_to_rownames
#' @importFrom directPA geneStats
PreProcess_Frank <- function(x_list, y_list, contrast = NULL, nFeatures = 50, combinationMethod = "OSP") {
    if (!combinationMethod %in% c("Stouffer", "OSP", "Fisher", "maxP")) {
        stop(print("Available methods are Stouffer, OSP, Fisher, or maxP"))
    }

    # Transpose x_list
    x_list <- lapply(x_list, t)

    tT <- list()
    for (i in 1:length(x_list)) { # nolint
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
        des <- model.matrix(~ 0 + y_list[[i]]) # nolint
        # Make the names of the columns in the design matrix the same as the levels of y_list
        colnames(des) <- levels(y_list[[i]])

        # Run limma
        fit <- limma::lmFit(x_list[[i]], design = des)
        CM <- limma::makeContrasts(contrasts = contrast, levels = des)
        fit2 <- limma::contrasts.fit(fit, CM)
        efit <- limma::eBayes(fit2, robust = TRUE)
        tT[[i]] <- limma::topTable(efit, coef = contrast, n = Inf) %>%
            dplyr::select(t) %>%
            data.frame() # nolint

        # Add a column with the gene names
        tT[[i]]$gene <- rownames(tT[[i]])
    }
    # merge a the tT list into a single data frame by gene
    suppressWarnings(
        tT <- Reduce(function(x, y) merge(x, y, by = "gene", all = TRUE), tT)
    )

    # Keep unique genes
    tT <- tT[!duplicated(tT$gene), ]
    # Move gene column to rownames
    tT <- tT %>%
        tibble::remove_rownames() %>%
        tibble::column_to_rownames(var = "gene")

    # Run directPA
    Z.Scores.All <- apply(tT, 2, function(x) {
        qnorm(rank(x) / (nrow(tT) + 1))
    })
    data(Pathways)
    gene.pvalues <- apply(Z.Scores.All, 1, function(x) {
        directPA::geneStats(x, method = combinationMethod)
    })
    gene.zscores <- qnorm(gene.pvalues, lower.tail = FALSE)
    pvalue2sided <- 2 * pnorm(-abs(gene.zscores))
    sig.genes <- names(pvalue2sided %>% sort())[1:nFeatures]
    return(sig.genes)
}
