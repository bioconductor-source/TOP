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
#' data(cpop_data_binary, package = 'CPOP')
#'
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#' x3 = cpop_data_binary$x3
#' y1 = cpop_data_binary$y1
#' y2 = cpop_data_binary$y2
#' y3 = cpop_data_binary$y3
#'
#' set.seed(23)
#' x_list <- list(x1,x2)
#' y_list <- list(factor(y1), factor(y2))
#'
#' fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
#' CPOP_coefPlot(fCPOP_model)

CPOP_coefPlot <- function(CPOP_model, nFeatures = 20, s = "lambda.min"){
  library(dplyr)
  library(ggplot2)
  library(tibble)
  as.matrix(glmnet::coef.glmnet(CPOP_model$model, s = s)) |>
    data.frame() |>
    tibble::rownames_to_column("Features") |>
    filter(lambda.min != 0) %>%
    filter(Features != "(Intercept)") %>%
    top_n(Features, n = nFeatures) %>%
    ggplot(aes(x = lambda.min, y = reorder(Features, abs(lambda.min)), fill = abs(lambda.min))) + geom_bar(stat = "identity") +
    theme_bw() + ylab("Features") + xlab("") + scale_fill_viridis_c(name = "Coefficient\nValue", option = "plasma")
}
