#' Créer des plis de validation croisée
#'
#' @param n Nombre d'observations.
#' @param k Nombre de plis.
#' @param seed Graine aléatoire.
#' @return Un vecteur d'entiers de 1 à `k`, un par observation.
#' @export
make_folds <- function(n, k = 5, seed = 123) {
  set.seed(seed)
  sample(rep(seq_len(k), length.out = n))
}

#' Validation croisée k-fold
#'
#' @param data Un data frame.
#' @param target Nom de la variable cible.
#' @param fit_fun Fonction d'ajustement, par ex. [fit_tree] ou [fit_forest].
#' @param folds Plis, issus de [make_folds()].
#' @param seed Graine aléatoire (pour rpart et randomForest).
#' @param ... Arguments passés à `fit_fun`.
#' @return Un data frame : une ligne par pli, une colonne par métrique.
#' @export
cross_validate <- function(data, target, fit_fun,
                           folds = make_folds(nrow(data)), seed = 123, ...) {
  set.seed(seed)
  res <- lapply(sort(unique(folds)), function(i) {
    model <- fit_fun(data[folds != i, ], target, ...)
    m <- evaluate_model(model, data[folds == i, ], target)
    m$confusion <- NULL
    as.data.frame(m)
  })
  do.call(rbind, res)
}

#' Comparer arbre de décision et forêt aléatoire
#'
#' @inheritParams cross_validate
#' @param ... Arguments passés à [fit_forest()] (par ex. `mtry`).
#' @return Un data frame : moyenne +/- écart-type de chaque métrique.
#' @export
compare_models <- function(data, target, folds = make_folds(nrow(data)), ...) {
  tree   <- cross_validate(data, target, fit_tree, folds)
  forest <- cross_validate(data, target, fit_forest, folds, ...)
  fmt <- function(x) sprintf("%.3f +/- %.3f", mean(x), stats::sd(x))
  data.frame(
    metrique = names(tree),
    arbre    = vapply(tree, fmt, character(1)),
    foret    = vapply(forest, fmt, character(1)),
    row.names = NULL
  )
}
