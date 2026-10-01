#' Fit a classification random forest
#'
#' @param data A data frame.
#' @param target Name of the outcome column (must be a factor).
#' @param ntree Number of trees.
#' @param ... Extra arguments passed to [randomForest::randomForest()].
#' @return A `randomForest` model.
#' @export
fit_forest <- function(data, target, ntree = 500, ...) {
  f <- stats::as.formula(paste(target, "~ ."))
  randomForest::randomForest(f, data = data, ntree = ntree,
                             importance = TRUE, ...)
}
