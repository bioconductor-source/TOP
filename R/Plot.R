#' TOP_coefPlot
#'
#' @param TOP_Model A Transferable Omics Prediction model. THe output from the TOP_model function.
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
#' model <- TOP_model(x_list, y_list)
#' TOP_coefPlot(model)
#'
#' @rdname TOP_coefPlot
#' @export
#' @import ggplot2
#' @importFrom glmnet coef.glmnet
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter top_n
#' @importFrom magrittr %>%
TOP_coefPlot <- function(TOP_model, nFeatures = 20, s = "lambda.min") {

    if (nFeatures > ncol(TOP_model$model$beta)) {
        stop("nFeatures must be less than or equal to the number of features in the model.")
    }

    as.matrix(glmnet::coef.glmnet(TOP_model$model, s = s)) %>%
        data.frame() %>%
        tibble::rownames_to_column("Features") %>%
        dplyr::filter(lambda.min != 0) %>%
        dplyr::filter(Features != "(Intercept)") %>%
        dplyr::top_n(lambda.min, n = nFeatures) %>%
        ggplot(
            aes(x = lambda.min, y = stats::reorder(Features, abs(lambda.min)),
            fill = abs(lambda.min))
        ) +
            geom_bar(stat = "identity") +
            theme_bw() +
            ylab("Features") +
            xlab("") +
            scale_fill_viridis_c(name = "Coefficient\nValue", option = "plasma")
}

#' TOP_lambdaPlot
#'
#' @param TOP_model A Transferable Omics Prediction model. THe output from the TOP_model function.
#' @param nFeatures The number of features to plot, features are ranked beta's for lambda.min. Default: 20
#' @param s Lambda value for the lasso model. Default is "lambda.min"
#' @param interactive A boolean indicaitng whether the plot should be interactive. Defaults to FALSE .
#' @param label A boolean indicating whether the features should be labeled on the plot. Defaults to FALSE .
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
#' model <- TOP_model(x_list, y_list)
#' CPOP_coefPlot(model)
#'
#' @import ggplot2
#' @importFrom tibble rownames_to_column
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate filter arrange top_n
#' @importFrom ggrepel geom_label_repel
#' @importFrom plotly ggplotly
#' @importFrom latex2exp TeX
#' @importFrom magrittr %>%
TOP_lambdaPlot <- function(TOP_model, nFeatures = 20, s = "lambda.min", interactive = FALSE, label = FALSE) {
    model <- TOP_model

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
            ggplot(aes(x = log, y = value, color = Feature, text = Feature)) +
            geom_line(size = 1.3) +
            theme_bw() +
            theme(legend.position = "none") +
            geom_vline(xintercept = log(lambda.min), linetype = "dashed") +
            geom_text(
                aes(x = log(lambda.min), label = "lambda.min", y = max(c$value)),
                angle = 0, color = "black", text = element_text(face = NULL),
                size = 6, hjust = -0.1
            )

    if (label) {
        p <- p + ggrepel::geom_label_repel(
            data = topfeatures, aes(label = Feature), size = 3.5,
            hjust = -0.1, nudge_x = 0.1, nudge_y = 0.1
        )
    }

    if (interactive) {
        return(plotly::ggplotly(p, tooltip = "text"))
    } else {
        return(
            p + xlab(latex2exp::TeX("log(${\\lambda}$)")) +
                ylab(latex2exp::TeX("${\\beta}$ Value"))
        )
    }
}

# Network plot of the CPOP model
#' simplenetworkPlot
#'
#' @param TOP_model A Transferable Omics Prediction model. THe output from the TOP_model function.
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
#' @importFrom magrittr %>%
simplenetworkPlot <- function(TOP_model, nFeatures = 50, s = "lambda.min") {
    # Create network and edge tables.
    network_tbl <- as.matrix(glmnet::coef.glmnet(TOP_model$models, s = s)) %>%
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
            ggraph::geom_node_text(aes(label = name), repel = T) + theme_void() +
            ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
}

# Network plot of the coefficients in the TOP model
#' coefNetworkPlot
#'
#' @param TOP_model A Transferable Omics Prediction model. THe output from the TOP_model function.
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
#' @importFrom ggraph ggraph
#' @importFrom igraph graph_from_data_frame
#' @importFrom magrittr %>%
coefNetworkPlot <- function(TOP_model, nFeatures = 20, s = "lambda.min"){
    ratio_df <- glmnet::coef.glmnet(TOP_model$model, s = s) %>%
        as.matrix %>%
        data.frame() %>%
        tibble::rownames_to_column("Features") %>%
        dplyr::filter(Features != "(Intercept)") %>%
        dplyr::mutate(score = abs(lambda.min)) %>%
        dplyr::top_n(score, n = nFeatures)

    edges_tbl = ratio_df %>%
        mutate(dir = ifelse(lambda.min > 0, "Pos", "Neg")) %>%
        tidyr::separate(col = "Features", into = c("from", "to"))

    ig = igraph::graph_from_data_frame(edges_tbl, directed = FALSE)

    ggraph(ig, layout = "linear", circular = TRUE) +
        ggraph::geom_edge_arc(aes(
            start_cap = label_rect(.data$node1.name),
            end_cap = label_rect(.data$node2.name),
            width = .data$score,
            color = .data$dir)) +
        theme_void() +
        ggraph::geom_node_text(aes(label = .data$name), size = 6) +
        ggraph::scale_edge_colour_brewer(palette = "Set1", direction = -1)
}

