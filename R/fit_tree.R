#' Fit a classification decision tree
#'
#' @param data A data frame.
#' @param target Name of the outcome column (must be a factor).
#' @param ... Extra arguments passed to [rpart::rpart()].
#' @return An `rpart` model.
#' @export
fit_tree <- function(data, target, ...) {
  f <- stats::as.formula(paste(target, "~ ."))
  rpart::rpart(f, data = data, method = "class", model = TRUE, ...)
}

#' Plot a decision tree
#'
#' @param model An `rpart` model from [fit_tree()].
#' @export
plot_tree <- function(model) {
  rpart.plot::rpart.plot(model)
}
