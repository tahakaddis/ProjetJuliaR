#' Validation croisée k-fold
#'
#' @param data Un data frame.
#' @param target Nom de la variable cible.
#' @param fit_fun Fonction d'ajustement, par ex. [fit_tree] ou [fit_forest].
#' @param k Nombre de plis.
#' @param metric Métrique : par défaut `"accuracy"` (classification)
#'   ou `"rmse"` (régression) ; aussi `"mae"` ou `"r2"`.
#' @param seed Graine aléatoire.
#' @param ... Arguments passés à `fit_fun`.
#' @return Un vecteur des valeurs par pli (moyenne et écart-type en attributs).
#' @export
cross_validate <- function(data, target, fit_fun, k = 5, metric = NULL,
                           seed = 123, ...) {
  if (is.null(metric)) {
    metric <- if (is.factor(data[[target]])) "accuracy" else "rmse"
  }
  set.seed(seed)
  folds <- sample(rep(seq_len(k), length.out = nrow(data)))
  res <- vapply(seq_len(k), function(i) {
    model <- fit_fun(data[folds != i, ], target, ...)
    evaluate_model(model, data[folds == i, ], target)[[metric]]
  }, numeric(1))
  attr(res, "mean") <- mean(res)
  attr(res, "sd") <- stats::sd(res)
  res
}
