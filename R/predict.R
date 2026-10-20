#' Prédictions d'un arbre de décision ou d'une forêt aléatoire
#'
#' Méthodes de la fonction générique [stats::predict()] pour les classes
#' `arbre_decision` et `foret_aleatoire`.
#'
#' @param object Un modèle issu de [arbre_decision()] ou [foret_aleatoire()].
#' @param newdata Data frame contenant les variables explicatives.
#' @param type Pour la classification : `"classe"` (défaut) ou `"proba"`.
#'   Ignoré en régression.
#' @param ... Non utilisé.
#' @return Classification : un facteur (`"classe"`) ou une matrice de
#'   probabilités (`"proba"`). Régression : un vecteur numérique.
#' @examples
#' arbre <- arbre_decision(Classification ~ ., data = coimbra)
#' head(predict(arbre, coimbra))
#' head(predict(arbre, coimbra, type = "proba"))
#' @name predict_modele
NULL

#' @rdname predict_modele
#' @export
predict.arbre_decision <- function(object, newdata, type = c("classe", "proba"), ...) {
  type <- match.arg(type)
  x <- preparer_newdata(object, newdata)
  K <- length(object$niveaux)
  sortie <- predire_noeud(object$racine, x, K)
  mettre_en_forme(object, sortie, type)
}

#' @rdname predict_modele
#' @export
predict.foret_aleatoire <- function(object, newdata, type = c("classe", "proba"), ...) {
  type <- match.arg(type)
  x <- preparer_newdata(object, newdata)
  K <- length(object$niveaux)
  n <- nrow(x)

  if (object$type == "classification") {
    # Chaque arbre vote pour une classe ; la "probabilité" est la part de votes.
    votes <- matrix(0, n, K)
    for (arbre in object$arbres) {
      classe <- max.col(predire_noeud(arbre$racine, x, K), ties.method = "first")
      votes[cbind(seq_len(n), classe)] <- votes[cbind(seq_len(n), classe)] + 1
    }
    mettre_en_forme(object, votes / object$ntree, type)
  } else {
    preds <- vapply(object$arbres, function(a) predire_noeud(a$racine, x, 0),
                    numeric(n))
    rowMeans(matrix(preds, nrow = n))
  }
}

# Parcours récursif : chaque observation descend jusqu'à sa feuille.
# Renvoie une matrice n x K de probabilités (classification) ou un vecteur
# de valeurs (régression, K = 0).
predire_noeud <- function(noeud, x, K) {
  n <- nrow(x)
  if (noeud$feuille) {
    if (K > 0) return(matrix(noeud$proba, n, K, byrow = TRUE))
    return(rep(noeud$prediction, n))
  }
  a_gauche <- x[, noeud$variable] < noeud$seuil
  sortie <- if (K > 0) matrix(0, n, K) else numeric(n)
  if (any(a_gauche)) {
    res <- predire_noeud(noeud$gauche, x[a_gauche, , drop = FALSE], K)
    if (K > 0) sortie[a_gauche, ] <- res else sortie[a_gauche] <- res
  }
  if (any(!a_gauche)) {
    res <- predire_noeud(noeud$droite, x[!a_gauche, , drop = FALSE], K)
    if (K > 0) sortie[!a_gauche, ] <- res else sortie[!a_gauche] <- res
  }
  sortie
}

preparer_newdata <- function(object, newdata) {
  manquantes <- setdiff(object$variables, names(newdata))
  if (length(manquantes) > 0) {
    stop("Variables absentes de `newdata` : ", paste(manquantes, collapse = ", "))
  }
  verifier_predicteurs(newdata[, object$variables, drop = FALSE])
}

mettre_en_forme <- function(object, sortie, type) {
  if (object$type == "regression") return(sortie)
  colnames(sortie) <- object$niveaux
  if (type == "proba") return(sortie)
  factor(object$niveaux[max.col(sortie, ties.method = "first")],
         levels = object$niveaux)
}
