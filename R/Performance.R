#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cpop_result PARAM_DESCRIPTION
#' @param newx PARAM_DESCRIPTION
#' @param covariates PARAM_DESCRIPTION, Default: NULL
#' @param s PARAM_DESCRIPTION, Default: 'lambda.min'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#'
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
    result_response <- predict(object = cpop_result, newx = w3, s = s,
                              type = "class")
  } else {
    result_response <- predict(object = cpop_result, newx = newz, s = s,
                              type = "class")
  }

  # Using caret to perform performance evaluation.
  cm <- caret::confusionMatrix(as.factor(result_response), y3)
  cm <- cm$byClass %>%
    tibble::enframe()
  colnames(cm) <- c("Evaluation", "Value")

  return(cm)
}
