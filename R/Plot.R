#' CPOP_coefPlot
#'
#' @param CPOP_Model A CPOP model
#' @param nFeatures The number of features that will be plotted. Default: 20
#' @param s Lambda value for the lasso model, Default: 'lambda.min'
#'
#' @return
#' @export
#'
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
#' fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
#' CPOP_coefPlot(fCPOP_model)
#'
#' @rdname CPOP_coefPlot
#' @export
#' @import ggplot2
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter top_n
CPOP_coefPlot <- function(CPOP_model, nFeatures = 20, s = "lambda.min") {

    as.matrix(glmnet::coef.glmnet(CPOP_model$model, s = s)) |>
        data.frame() |>
        tibble::rownames_to_column("Features") |>
        dplyr::filter(lambda.min != 0) |>
        dplyr::filter(Features != "(Intercept)") |>
        dplyr::top_n(lambda.min, n = nFeatures) |>
        ggplot2::ggplot(
            ggplot2::aes(x = lambda.min, y = stats::reorder(Features, abs(lambda.min)),
            fill = abs(lambda.min))
        ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::theme_bw() +
            ggplot2::ylab("Features") +
            ggplot2::xlab("") +
            ggplot2::scale_fill_viridis_c(name = "Coefficient\nValue", option = "plasma")
}

#' CPOP_lambdaPlot
#'
#' @param CPOP_model A CPOP model
#' @param nFeatures The number of features to plot, features are ranked beta's for lambda.min. Default: 20
#' @param s Lambda value for the lasso model. Default is "lambda.min"
#'
#' @return
#' @export
#'
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
#' fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
#' CPOP_coefPlot(fCPOP_model)
#'
#' @import ggplot2
#' @importFrom tibble rownames_to_column
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate filter arrange top_n
#' @importFrom ggrepel geom_label_repel
#' @importFrom plotly ggplotly
#' @importFrom latex2exp TeX
CPOP_lambdaPlot <- function(CPOP_model, nFeatures = 20, s = "lambda.min", interactive = FALSE, label = FALSE) {
    model <- CPOP_model

    lambda <- model$models$lambda
    lambda.min <- model$models$lambda.min

    c <- as.matrix(model$models$glmnet.fit$beta) %>%
        data.frame() %>%
        tibble::rownames_to_column("Feature") %>%
        reshape2::melt()

    names(lambda) <- levels(c$variable)

    df <- c %>%
        dplyr::mutate(lambda = lambda[variable]) %>%
        dplyr::mutate(log = log(lambda))

    topfeatures <- df %>%
        dplyr::filter(lambda == lambda.min) %>%
        dplyr::arrange(dplyr::desc(abs(value))) %>%
        dplyr::top_n(abs(value), n = nFeatures)

    p <- df %>%
        dplyr::filter(Feature %in% topfeatures$Feature) %>%
            ggplot2::ggplot(ggplot2::aes(x = log, y = value, color = Feature, text = Feature)) +
            ggplot2::geom_line(size = 1.3) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::geom_vline(xintercept = log(lambda.min), linetype = "dashed") +
            ggplot2::geom_text(
                ggplot2::aes(x = log(lambda.min), label = "lambda.min", y = max(c$value)),
                angle = 0, color = "black", text = ggplot2::element_text(face = NULL),
                size = 6, hjust = -0.1
            )

    if (label) {
        p <- p + ggrepel::geom_label_repel(
            data = topfeatures, ggplot2::aes(label = Feature), size = 3.5,
            hjust = -0.1, nudge_x = 0.1, nudge_y = 0.1
        )
    }

    if (interactive) {
        return(plotly::ggplotly(p, tooltip = "text"))
    } else {
        return(
            p + ggplot2::xlab(latex2exp::TeX("log(${\\lambda}$)")) +
                ggplot2::ylab(latex2exp::TeX("${\\beta}$ Value"))
        )
    }
}

# Network plot of the CPOP model
#' CPOP_simplenetworkPlot
#'
#' @param CPOP_model A CPOP model
#' @param nFeatures The number of features that will be plotted. Default: 20
#' @param s Lambda value for the lasso model. Default is "lambda.min"
#'
#' @return
#' @export
#'
#' @examples
#'  # TODO: add examples.
#' @import ggplot2
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter mutate top_n select
#' @importFrom tidyr separate
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph
#' @importFrom ggnewscale new_scale_fill new_scale_color
CPOP_simplenetworkPlot <- function(CPOP_model, nFeatures = 50, s = "lambda.min") {
    # Create network and edge tables.
    network_tbl <- as.matrix(glmnet::coef.glmnet(CPOP_model$models, s = s)) %>%
        data.frame() %>%
        tibble::rownames_to_column("Features") %>%
        dplyr::filter(Features != "(Intercept)") %>%
        dplyr::filter(lambda.min != 0) %>%
        dplyr::mutate(
            Direction = ifelse(lambda.min > 0, "Pos", "Neg"),
            coef_abs = abs(lambda.min)
        ) %>%
        dplyr::top_n(coef_abs, n = nFeatures)

    edges_tbl <- network_tbl %>%
        tidyr::separate(col = "Features", into = c("from", "to"))

    # Create a network plot in ggplot

    edges_tbl %>%
        dplyr::select(from, to, lambda.min) %>%
        tidygraph::as_tbl_graph(directed = TRUE) %>%
        ggraph::ggraph(layout = "kk") + ggraph::geom_edge_link(color = "black") +
            ggraph::geom_node_point(colour = "lightblue", size = 3) +
            ggraph::geom_node_text(ggplot2::aes(label = name), repel = T) + ggplot2::theme_void() +
            ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
}

# I will need to add a complexNetworkPlot function. Using the enrichr package to plot the network.

# Plot the survival curves for the CPOP model
CPOP_KaplanMeierPlot <- function(CPOP_model, s = "lambda.min") {
    # TODO
}
