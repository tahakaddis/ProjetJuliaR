#' Choisir mtry pour la forêt aléatoire
#'
#' @inheritParams cross_validate
#' @param mtry_values Valeurs de `mtry` à tester.
#' @param metric Métrique à moyenner : par défaut `"auc"` (classification)
#'   ou `"rmse"` (régression).
#' @param ntree Nombre d'arbres.
#' @return Un data frame avec `mtry` et le score moyen en validation croisée.
#' @export
tune_forest <- function(data, target, mtry_values,
                        folds = make_folds(nrow(data)),
                        metric = NULL, ntree = 500) {
  if (is.null(metric)) {
    metric <- if (is.factor(data[[target]])) "auc" else "rmse"
  }
  score <- vapply(mtry_values, function(m) {
    cv <- cross_validate(data, target, fit_forest, folds,
                         ntree = ntree, mtry = m)
    mean(cv[[metric]])
  }, numeric(1))
  data.frame(mtry = mtry_values, score = score)
}
