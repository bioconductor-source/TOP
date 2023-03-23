#' @title performance_TOP
#' @description A function to calculate the external performance of the Tranferable Omics Prediction model.
#' @param TOP_model This is the output of the function TOP_model.
#' @param newx A matrix of the new data to be predicted. With the same number of feature columns as the original data.
#' @param newy A vector of the true labels that are being predicted. With the same number of samples as newx.
#' @param covariates A data.frame of the same covariates as the original model, Default: NULL
#' @param s Lambda used in the lasso model, Default: 'lambda.min'
#' @return A confusion matrix that displays the performance of the classifier.
#' @examples
#'
#'
#' data(cpop_data_binary, package = "CPOP")
#' x1 = cpop_data_binary$x1
#' x2 = cpop_data_binary$x2
#'
#' x_list <- list(x1,x2)
#' y_list <- list(cpop_data_binary$y1, cpop_data_binary$y2)
#'
#' model <- TOP_model(x_list, y_list)
#'
#' x3 = cpop_data_binary$x3
#' y3 = cpop_data_binary$y3
#'
#' performance_TOP(model$models, newx = x3, newy = y3)
#'
#' @rdname performance_TOP
#' @export
#' @importFrom CPOP pairwise_col_diff
#' @importFrom glmnet makeX
#' @importFrom caret confusionMatrix
#' @importFrom tibble enframe
performance_TOP <- function(
  TOP_model, newx, newy, covariates = NULL, s = "lambda.min"
) {
  # Go through and make predictions
  newz <- CPOP::pairwise_col_diff(newx)
  if (!is.null(covariates)) {
    w3 <- glmnet::makeX(cbind(newz, covariates))
    result_response <- stats::predict(object = TOP_model, newx = w3, s = s,
                              type = "class")
  } else {
    result_response <- stats::predict(object = TOP_model, newx = newz, s = s,
                              type = "class")
  }

  # Using caret to perform performance evaluation.
  cm <- caret::confusionMatrix(as.factor(result_response), newy)
  cm <- cm$byClass |>
    tibble::enframe()
  colnames(cm) <- c("Evaluation", "Value")

  return(cm)
}
