#' Title
#'
#' @param CPOP_Model A model
#' @param nFeatures The number of features that will be plotted
#'
#' @return
#' @export
#'
#' @examples
#' library(CPOP)
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
#' fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
#' CPOP_coefPlot(fCPOP_model)
#'
CPOP_coefPlot <- function(CPOP_model, nFeatures = 20, s = "lambda.min") {
    library(dplyr)
    library(ggplot2)
    library(tibble)
    as.matrix(glmnet::coef.glmnet(CPOP_model$model, s = s)) |>
        data.frame() |>
        tibble::rownames_to_column("Features") |>
        filter(lambda.min != 0) |>
        filter(Features != "(Intercept)") |>
        top_n(lambda.min, n = nFeatures) |>
        ggplot(aes(x = lambda.min, y = reorder(Features, abs(lambda.min)), fill = abs(lambda.min))) +
        geom_bar(stat = "identity") +
        theme_bw() +
        ylab("Features") +
        xlab("") +
        scale_fill_viridis_c(name = "Coefficient\nValue", option = "plasma")
}

#' Title
#'
#' @param CPOP_model A CPOP model
#' @param nFeatures The number of features to plot, features are ranked beta's for lambda.min
#' @param s lambda value. Default is "lambda.min"
#'
#' @return
#' @export
#'
#' @examples
#' library(CPOP)
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
#' fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
#' CPOP_coefPlot(fCPOP_model)
#'
CPOP_lambdaPlot <- function(CPOP_model, nFeatures = 20, s = "lambda.min", interactive = FALSE, label = FALSE) {
    model <- CPOP_model

    lambda <- model$models$lambda
    lambda.min <- model$models$lambda.min

    c <- as.matrix(model$models$glmnet.fit$beta) %>%
        data.frame() %>%
        rownames_to_column("Feature") %>%
        reshape2::melt()

    names(lambda) <- levels(c$variable)

    library(latex2exp)
    df <- c %>%
        mutate(lambda = lambda[variable]) %>%
        mutate(log = log(lambda))

    topfeatures <- df %>%
        filter(lambda == lambda.min) %>%
        arrange(desc(abs(value))) %>%
        top_n(abs(value), n = nFeatures)

    p <- df %>%
        filter(Feature %in% topfeatures$Feature) %>%
        ggplot(aes(x = log, y = value, color = Feature, text = Feature)) +
        geom_line(size = 1.3) +
        theme_bw() +
        theme(legend.position = "none") +
        geom_vline(xintercept = log(lambda.min), linetype = "dashed") +
        geom_text(aes(x = log(lambda.min), label = "lambda.min", y = max(c$value)),
            angle = 0, color = "black", text = element_text(face = NULL), size = 6, hjust = -0.1
        )

    if (label) {
        library(ggrepel)
        p <- p + ggrepel::geom_label_repel(
            data = topfeatures, aes(label = Feature), size = 3.5,
            hjust = -0.1, nudge_x = 0.1, nudge_y = 0.1
        )
    }

    if (interactive) {
        library(plotly)
        return(ggplotly(p, tooltip = "text"))
    } else {
        return(p + xlab(latex2exp::TeX("log(${\\lambda}$)")) + ylab(latex2exp::TeX("${\\beta}$ Value")))
    }
}

# Network plot of the CPOP model
#' Title
#'
#' @param CPOP_model A CPOP model
#' @param nFeatures The number of features that will be plotted
#' @param s lambda value. Default is "lambda.min"
#'
#' @return
#' @export
#'
#' @examples
CPOP_simplenetworkPlot <- function(CPOP_model, nFeatures = 50, s = "lambda.min") {
    # Create network and edge tables.
    network_tbl <- as.matrix(glmnet::coef.glmnet(fCPOP_Model$models, s = s)) %>%
        data.frame() %>%
        tibble::rownames_to_column("Features") %>%
        filter(Features != "(Intercept)") %>%
        filter(lambda.min != 0) %>%
        mutate(
            Direction = ifelse(lambda.min > 0, "Pos", "Neg"),
            coef_abs = abs(lambda.min)
        ) %>%
        top_n(coef_abs, n = nFeatures)

    edges_tbl <- network_tbl %>%
        tidyr::separate(col = "Features", into = c("from", "to"))
    # Create a network plot in ggplot
    library(ggraph)
    edges_tbl %>%
        dplyr::select(from, to, lambda.min) %>%
        tidygraph::as_tbl_graph(directed = TRUE) %>%
        ggraph(layout = "kk") + geom_edge_link(color = "black") + geom_node_point(colour = "lightblue", size = 3) +
        geom_node_text(aes(label = name), repel = T) + theme_void() + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
}

# I will need to add a complexNetworkPlot function. Using the enrichr package to plot the network.

# Plot the survival curves for the CPOP model
CPOP_KaplanMeierPlot <- function(CPOP_model, s = "lambda.min") {
    #
}
