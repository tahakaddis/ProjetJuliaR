#' Créer des plis de validation croisée
#'
#' Avec `k = 5` et `graine = 123`, on retrouve exactement les plis du fichier
#' `data-raw/folds.csv` partagé avec la partie Julia.
#'
#' @param n Nombre d'observations.
#' @param k Nombre de plis.
#' @param graine Graine aléatoire.
#' @return Un vecteur d'entiers de 1 à `k`, un par observation.
#' @export
creer_plis <- function(n, k = 5, graine = 123) {
  set.seed(graine)
  sample(rep(seq_len(k), length.out = n))
}

#' Validation croisée k-fold
#'
#' Pour chaque pli `k`, le modèle est entraîné sur les autres plis puis
#' évalué par [evaluer()] sur le pli `k`.
#'
#' @param formule Formule du modèle, par ex. `Classification ~ .`.
#' @param donnees Data frame.
#' @param methode Fonction de construction du modèle : [arbre_decision] ou
#'   [foret_aleatoire] (ou toute fonction acceptant `formule, data, ...`).
#' @param plis Plis, par ex. issus de [creer_plis()].
#' @param graine Graine aléatoire (pour le hasard des forêts).
#' @param ... Paramètres passés à `methode` (par ex. `mtry`, `cp`).
#' @return Un data frame de classe `resultat_cv` : une ligne par pli, une
#'   colonne par métrique.
#' @export
validation_croisee <- function(formule, donnees, methode = arbre_decision,
                               plis = creer_plis(nrow(donnees)),
                               graine = 123, ...) {
  set.seed(graine)
  resultats <- lapply(sort(unique(plis)), function(k) {
    modele <- methode(formule, data = donnees[plis != k, , drop = FALSE], ...)
    as.data.frame(evaluer(modele, donnees[plis == k, , drop = FALSE]))
  })
  res <- do.call(rbind, resultats)
  rownames(res) <- paste("pli", sort(unique(plis)))
  class(res) <- c("resultat_cv", class(res))
  res
}

#' @rdname validation_croisee
#' @param x Un objet de classe `resultat_cv`.
#' @export
print.resultat_cv <- function(x, ...) {
  print(round(as.data.frame(unclass(x), row.names = rownames(x)), 3))
  cat("\nMoyenne +/- \u00e9cart-type\n")
  print(resumer_cv(x), row.names = FALSE)
  invisible(x)
}

#' @rdname validation_croisee
#' @param object Un objet de classe `resultat_cv`.
#' @export
summary.resultat_cv <- function(object, ...) {
  data.frame(metrique = names(object),
             moyenne = vapply(object, mean, numeric(1)),
             ecart_type = vapply(object, stats::sd, numeric(1)),
             row.names = NULL)
}

resumer_cv <- function(x) {
  s <- summary.resultat_cv(x)
  data.frame(metrique = s$metrique,
             valeur = sprintf("%.3f +/- %.3f", s$moyenne, s$ecart_type))
}

#' Comparer arbre de décision et forêt aléatoire
#'
#' Lance [validation_croisee()] avec les **mêmes plis** pour un arbre et une
#' forêt, et présente les résultats côte à côte.
#'
#' @inheritParams validation_croisee
#' @param args_arbre,args_foret Listes de paramètres passés à
#'   [arbre_decision()] et [foret_aleatoire()].
#' @return Un data frame : moyenne +/- écart-type de chaque métrique.
#' @export
comparer_modeles <- function(formule, donnees, plis = creer_plis(nrow(donnees)),
                             args_arbre = list(), args_foret = list()) {
  cv <- function(methode, args) {
    do.call(validation_croisee,
            c(list(formule = formule, donnees = donnees, methode = methode,
                   plis = plis), args))
  }
  arbre <- resumer_cv(cv(arbre_decision, args_arbre))
  foret <- resumer_cv(cv(foret_aleatoire, args_foret))
  data.frame(metrique = arbre$metrique, arbre = arbre$valeur,
             foret = foret$valeur)
}

#' Régler un paramètre par validation croisée
#'
#' Teste plusieurs valeurs d'un paramètre (par ex. `mtry` pour une forêt,
#' `profondeur_max` ou `cp` pour un arbre) et garde la meilleure.
#'
#' @inheritParams validation_croisee
#' @param parametre Nom du paramètre à régler (chaîne de caractères).
#' @param valeurs Valeurs à tester.
#' @param metrique Métrique à optimiser. Par défaut `"auc"` en
#'   classification (à maximiser) et `"rmse"` en régression (à minimiser).
#' @return Un data frame de classe `resultat_reglage` ; la meilleure valeur
#'   est dans l'attribut `"meilleure"`.
#' @export
regler <- function(formule, donnees, methode, parametre, valeurs,
                   plis = creer_plis(nrow(donnees)), metrique = NULL, ...) {
  y <- stats::model.response(stats::model.frame(formule, donnees))
  if (is.null(metrique)) metrique <- if (is.factor(y)) "auc" else "rmse"
  minimiser <- metrique %in% c("rmse", "mae")

  scores <- vapply(valeurs, function(v) {
    args <- list(...)
    args[[parametre]] <- v
    cv <- do.call(validation_croisee,
                  c(list(formule = formule, donnees = donnees,
                         methode = methode, plis = plis), args))
    mean(cv[[metrique]])
  }, numeric(1))

  res <- data.frame(valeurs, scores)
  names(res) <- c(parametre, metrique)
  meilleur <- if (minimiser) which.min(scores) else which.max(scores)
  structure(res, meilleure = valeurs[meilleur],
            class = c("resultat_reglage", "data.frame"))
}

#' @rdname regler
#' @param x Un objet de classe `resultat_reglage`.
#' @export
print.resultat_reglage <- function(x, ...) {
  print(as.data.frame(unclass(x))[, 1:2], row.names = FALSE, digits = 4)
  cat("\nMeilleure valeur de", names(x)[1], ":", attr(x, "meilleure"), "\n")
  invisible(x)
}
