#' Evaluate a classification model on test data
#'
#' @param model A model from [fit_tree()] or [fit_forest()].
#' @param test Test data frame.
#' @param target Name of the outcome column.
#' @return A list with the confusion matrix and the accuracy.
#' @export
evaluate_model <- function(model, test, target) {
  pred <- if (inherits(model, "rpart")) {
    stats::predict(model, newdata = test, type = "class")
  } else {
    stats::predict(model, newdata = test)
  }
  cm <- table(predicted = pred, actual = test[[target]])
  list(confusion = cm, accuracy = sum(diag(cm)) / sum(cm))
}
