#' @title performance_cpop2
#' @description A function to calculate the external performance of the CPOP2 model. 
#' @param cpop_result A model of class CPOP2. This is the output of the function Frankenstein_CPOP.
#' @param newx A matrix of the new data to be predicted. With the same number of feature columns as the original data.
#' @param covariates A data.frame of the same covariates as the original model, Default: NULL
#' @param s Lambda used in the lasso model, Default: 'lambda.min'
#' @return OUTPUT_DESCRIPTION
#' @examples
#'  # TODO
#' @rdname performance_cpop2
#' @export
#' @importFrom CPOP pairwise_col_diff
#' @importFrom glmnet makeX
#' @importFrom caret confusionMatrix
#' @importFrom tibble enframe
performance_cpop2 <- function(
  cpop_result, newx, covariates = NULL, s = "lambda.min"
) {
  # Go through and make predictions
  newz <- CPOP::pairwise_col_diff(newx)
  if (!is.null(covariates)) {
    w3 <- glmnet::makeX(cbind(newz, covariates))
    result_response <- stats::predict(object = cpop_result, newx = w3, s = s,
                              type = "class")
  } else {
    result_response <- stats::predict(object = cpop_result, newx = newz, s = s,
                              type = "class")
  }

  # Using caret to perform performance evaluation.
  cm <- caret::confusionMatrix(as.factor(result_response), y3)
  cm <- cm$byClass |>
    tibble::enframe()
  colnames(cm) <- c("Evaluation", "Value")

  return(cm)
}
