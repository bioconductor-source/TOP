#' @title performance_TOP
#' @description A function to calculate the external performance of the Tranferable Omics Prediction model. 
#' @param TOP_model This is the output of the function TOP_model.
#' @param newx A matrix of the new data to be predicted. With the same number of feature columns as the original data.
#' @param covariates A data.frame of the same covariates as the original model, Default: NULL
#' @param s Lambda used in the lasso model, Default: 'lambda.min'
#' @return A confusion matrix that 
#' @examples
#'  # TODO
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
