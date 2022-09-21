library(limma)
library(dplyr)
library(directPA)
library(PhosR)
library(tibble)

PreProcess_Frank <- function(x_list, y_list, contrast = NULL, gene_number = 50) {

    # Transpose x_list
    x_list <- lapply(x_list, t)

    tT <- list()
    for(i in 1:length(x_list)) { # nolint
        # Assign levels to y_list if contrasts are not given.
        if (is.null(contrast)) {
            level <- levels(y_list[[i]])
            contrast = level[2] - level[1]
        }

        # Make sure y_list is a factor.
        y_list[[i]] <- factor(y_list[[i]])
        # Make sure x_list is a matrix.
        x_list[[i]] <- as.matrix(x_list[[i]])

        # Set up a design matrix according to y_list
        des <- model.matrix(~0 + y_list[[i]]) # nolint
        # Make the names of the columns in the design matrix the same as the levels of y_list
        colnames(des) <- levels(y_list[[i]])

        # Run limma
        fit <- lmFit(x_list[[i]], design = des)
        CM <- makeContrasts(contrasts = contrast, levels = des)
        fit2 <- contrasts.fit(fit, CM)
        efit <- eBayes(fit2, robust = TRUE)
        tT[[i]] <- topTable(efit, coef = contrast, n = Inf) %>% dplyr::select(t) %>% data.frame() # nolint

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
    tT <- tibble::column_to_rownames(tT, var = "gene")

    # Run directPA
    Z.Scores.All <- apply(tT, 2, function(x){qnorm(rank(x)/(nrow(tT)+1))})
    data(Pathways)
    gene.pvalues <- apply(Z.Scores.All, 1, geneStats)
    gene.zscores <- qnorm(gene.pvalues, lower.tail = FALSE)
    pvalue2sided=2*pnorm(-abs(gene.zscores))
    sig.genes <- names(pvalue2sided %>% sort())[1:gene_number]
    return(sig.genes)
}