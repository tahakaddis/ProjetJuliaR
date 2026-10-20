#' Évaluer un modèle sur des données de test
#'
#' Fonction générique. Le **dispatch** se fait sur la classe de tâche du
#' modèle, partagée par les arbres et les forêts :
#'
#' * `evaluer.classification()` : matrice de confusion, accuracy,
#'   sensibilité, spécificité et AUC (classe positive = second niveau du
#'   facteur, ici `"cancer"`) ;
#' * `evaluer.regression()` : RMSE, MAE et R².
#'
#' Dans les deux cas, les prédictions sont obtenues par `predict()`, qui
#' dispatche à son tour sur `arbre_decision` ou `foret_aleatoire`.
#'
#' @param modele Un modèle issu de [arbre_decision()] ou [foret_aleatoire()].
#' @param donnees Data frame de test contenant la variable cible.
#' @param ... Non utilisé.
#' @return Un objet de classe `metriques` (avec ses méthodes `print()` et
#'   `as.data.frame()`).
#' @examples
#' arbre <- arbre_decision(Classification ~ ., data = coimbra)
#' evaluer(arbre, coimbra)
#' @export
evaluer <- function(modele, donnees, ...) {
  UseMethod("evaluer")
}

#' @rdname evaluer
#' @export
evaluer.classification <- function(modele, donnees, ...) {
  y <- extraire_cible(modele, donnees)
  proba <- stats::predict(modele, donnees, type = "proba")
  pred <- factor(modele$niveaux[max.col(proba, ties.method = "first")],
                 levels = modele$niveaux)
  confusion <- table(predit = pred, observe = y)
  res <- list(confusion = confusion,
              accuracy = sum(diag(confusion)) / sum(confusion))

  if (length(modele$niveaux) == 2) {
    neg <- modele$niveaux[1]
    pos <- modele$niveaux[2]
    n_pos <- sum(y == pos)
    n_neg <- sum(y == neg)
    res$sensibilite <- confusion[pos, pos] / n_pos
    res$specificite <- confusion[neg, neg] / n_neg
    res$auc <- auc_mann_whitney(proba[, pos], y == pos)
  }
  structure(res, class = c("metriques_classification", "metriques"))
}

#' @rdname evaluer
#' @export
evaluer.regression <- function(modele, donnees, ...) {
  y <- extraire_cible(modele, donnees)
  erreur <- y - stats::predict(modele, donnees)
  structure(
    list(rmse = sqrt(mean(erreur^2)),
         mae = mean(abs(erreur)),
         r2 = 1 - sum(erreur^2) / sum((y - mean(y))^2)),
    class = c("metriques_regression", "metriques")
  )
}

#' @rdname evaluer
#' @param x Un objet de classe `metriques`.
#' @export
print.metriques <- function(x, ...) {
  valeurs <- unlist(x[vapply(x, function(v) is.numeric(v) && length(v) == 1, logical(1))])
  print(round(valeurs, 3))
  invisible(x)
}

#' @rdname evaluer
#' @export
print.metriques_classification <- function(x, ...) {
  cat("Matrice de confusion\n")
  print(x$confusion)
  cat("\n")
  NextMethod()
}

#' @rdname evaluer
#' @param row.names,optional Ignorés (requis par la générique).
#' @export
as.data.frame.metriques <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$confusion <- NULL
  as.data.frame(unclass(x))
}

# AUC = probabilité qu'un cas positif ait un score plus élevé qu'un cas
# négatif (statistique de Mann-Whitney, ex aequo comptés pour 1/2).
auc_mann_whitney <- function(score, positif) {
  n_pos <- sum(positif)
  n_neg <- sum(!positif)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  rangs <- rank(score)
  (sum(rangs[positif]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

extraire_cible <- function(modele, donnees) {
  if (!is.null(modele$formule)) {
    return(stats::model.response(stats::model.frame(modele$formule, donnees)))
  }
  if (!modele$cible %in% names(donnees)) {
    stop("La variable cible `", modele$cible, "` est absente des donn\u00e9es.")
  }
  donnees[[modele$cible]]
}
