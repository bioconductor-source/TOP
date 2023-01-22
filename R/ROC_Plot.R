#' @title ROC_Plot
#' @description A function visualises the performance of a classifier by plotting the Receiver Operating Characteristic (ROC) curve.
#' @param roc_list A list of roc objects from the pROC package
#'
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
#' pred <- predict_cpop2(model$models, newx = x3)
#' roc <- pROC::roc(y3, pred)
#' ROC_Plot(list(roc))
#'
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @importFrom ggthemes scale_color_tableau
ROC_Plot <- function(roc_list) {
    data.labels <- extractAUC(roc_list)

    pROC::ggroc(roc_list, size = 1.5) + theme_bw() +
        ggthemes::scale_color_tableau(
            name = "Model", labels = data.labels$label_long
        ) +
        geom_segment(
            aes(x = 1, xend = 0, y = 0, yend = 1),
            color = "grey50", linetype = "dashed"
        ) +
        theme(
            legend.title = element_text(size = 14)) +
            theme(legend.text = element_text(size = 12)
        ) +
        ggtitle("")
}
