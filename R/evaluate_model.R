#' Évaluer un modèle sur des données de test
#'
#' Pour une classification binaire, la classe positive est le second
#' niveau du facteur (ici "cancer").
#'
#' @param model Un modèle issu de [fit_tree()] ou [fit_forest()].
#' @param test Data frame de test.
#' @param target Nom de la variable cible.
#' @return Classification : matrice de confusion, accuracy, sensibilité,
#'   spécificité et AUC. Régression : RMSE, MAE et R².
#' @export
evaluate_model <- function(model, test, target) {
  y <- test[[target]]

  if (is.factor(y)) {
    type <- if (inherits(model, "rpart")) "class" else "response"
    pred <- factor(stats::predict(model, newdata = test, type = type),
                   levels = levels(y))
    cm <- table(predicted = pred, actual = y)
    res <- list(confusion = cm, accuracy = sum(diag(cm)) / sum(cm))

    if (nlevels(y) == 2) {
      neg <- levels(y)[1]
      pos <- levels(y)[2]
      prob <- stats::predict(model, newdata = test, type = "prob")[, pos]
      n_pos <- sum(y == pos)
      n_neg <- sum(y == neg)
      res$sensitivity <- cm[pos, pos] / n_pos
      res$specificity <- cm[neg, neg] / n_neg
      # AUC (formule de Mann-Whitney)
      r <- rank(prob)
      res$auc <- (sum(r[y == pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
    }
    return(res)
  }

  pred <- stats::predict(model, newdata = test)
  err <- y - pred
  list(
    rmse = sqrt(mean(err^2)),
    mae  = mean(abs(err)),
    r2   = 1 - sum(err^2) / sum((y - mean(y))^2)
  )
}
