#' Évaluer un modèle sur des données de test
#'
#' @param model Un modèle issu de [fit_tree()] ou [fit_forest()].
#' @param test Data frame de test.
#' @param target Nom de la variable cible.
#' @return Classification : matrice de confusion et accuracy.
#'   Régression : RMSE, MAE et R².
#' @export
evaluate_model <- function(model, test, target) {
  y <- test[[target]]

  if (is.factor(y)) {
    pred <- if (inherits(model, "rpart")) {
      stats::predict(model, newdata = test, type = "class")
    } else {
      stats::predict(model, newdata = test)
    }
    cm <- table(predicted = pred, actual = y)
    return(list(confusion = cm, accuracy = sum(diag(cm)) / sum(cm)))
  }

  pred <- stats::predict(model, newdata = test)
  err <- y - pred
  list(
    rmse = sqrt(mean(err^2)),
    mae  = mean(abs(err)),
    r2   = 1 - sum(err^2) / sum((y - mean(y))^2)
  )
}
