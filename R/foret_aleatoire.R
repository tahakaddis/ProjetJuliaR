#' Forêt aléatoire
#'
#' Construit une forêt aléatoire (Breiman, 2001) à partir d'arbres CART
#' implémentés par [arbre_decision()] :
#'
#' 1. chaque arbre est entraîné sur un **échantillon bootstrap** (tirage avec
#'    remise de `n` observations) ;
#' 2. à chaque nœud, seules `mtry` variables tirées au hasard sont candidates ;
#' 3. les arbres sont développés sans élagage ;
#' 4. la prédiction est le **vote majoritaire** (classification) ou la
#'    **moyenne** (régression) des arbres.
#'
#' Les observations non tirées par un arbre (*out-of-bag*, OOB) servent à
#' estimer l'erreur de la forêt sans données de test.
#'
#' Comme [arbre_decision()], `foret_aleatoire()` est une fonction générique
#' avec une méthode `formula` et une méthode `default`.
#'
#' @param x Une formule (`cible ~ .`) ou un data frame de prédicteurs
#'   numériques.
#' @param ... Arguments passés aux méthodes.
#' @return Un objet S3 de classe
#'   `c("foret_aleatoire", "classification" | "regression", "modele_arbre")`.
#' @examples
#' foret <- foret_aleatoire(Classification ~ ., data = coimbra, ntree = 100)
#' foret
#' importance(foret)
#' @export
foret_aleatoire <- function(x, ...) {
  UseMethod("foret_aleatoire")
}

#' @rdname foret_aleatoire
#' @param data Data frame contenant les variables de la formule.
#' @export
foret_aleatoire.formula <- function(x, data, ...) {
  mf <- stats::model.frame(x, data)
  modele <- foret_aleatoire.default(mf[, -1, drop = FALSE],
                                    stats::model.response(mf), ...)
  modele$formule <- x
  modele$cible <- names(mf)[1]
  modele
}

#' @rdname foret_aleatoire
#' @param y Variable cible : facteur (classification) ou numérique
#'   (régression).
#' @param ntree Nombre d'arbres.
#' @param mtry Nombre de variables candidates à chaque nœud. Par défaut
#'   `floor(sqrt(p))` en classification et `floor(p / 3)` en régression.
#' @param min_feuille Taille minimale des feuilles. Par défaut 1 en
#'   classification et 5 en régression.
#' @export
foret_aleatoire.default <- function(x, y, ntree = 500, mtry = NULL,
                                    min_feuille = NULL, ...) {
  x <- verifier_predicteurs(x)
  n <- nrow(x)
  p <- ncol(x)
  classification <- is.factor(y)
  if (is.null(mtry)) {
    mtry <- if (classification) floor(sqrt(p)) else max(floor(p / 3), 1)
  }
  if (is.null(min_feuille)) min_feuille <- if (classification) 1 else 5
  parametres <- list(profondeur_max = Inf, min_noeud = 2 * min_feuille,
                     min_feuille = min_feuille, cp = 0, mtry = min(mtry, p))

  arbres <- vector("list", ntree)
  K <- if (classification) nlevels(y) else 0
  votes_oob <- matrix(0, n, max(K, 1))       # classification
  somme_oob <- numeric(n)                    # régression
  nb_oob <- numeric(n)

  for (b in seq_len(ntree)) {
    tirage <- sample.int(n, n, replace = TRUE)
    arbres[[b]] <- ajuster_arbre(x[tirage, , drop = FALSE], y[tirage], parametres)

    oob <- setdiff(seq_len(n), tirage)
    if (length(oob) == 0) next
    pred <- predire_noeud(arbres[[b]]$racine, x[oob, , drop = FALSE], K)
    nb_oob[oob] <- nb_oob[oob] + 1
    if (classification) {
      classe <- max.col(pred, ties.method = "first")
      votes_oob[cbind(oob, classe)] <- votes_oob[cbind(oob, classe)] + 1
    } else {
      somme_oob[oob] <- somme_oob[oob] + pred
    }
  }

  vus <- nb_oob > 0
  if (classification) {
    pred_oob <- max.col(votes_oob, ties.method = "first")
    erreur_oob <- mean(pred_oob[vus] != as.integer(y)[vus])
  } else {
    erreur_oob <- mean((somme_oob[vus] / nb_oob[vus] - y[vus])^2)
  }

  importance <- Reduce(`+`, lapply(arbres, `[[`, "importance")) / ntree

  structure(
    list(arbres = arbres, type = if (classification) "classification" else "regression",
         niveaux = if (classification) levels(y) else NULL,
         variables = colnames(x), parametres = parametres, ntree = ntree,
         erreur_oob = erreur_oob, importance = importance, n = n,
         cible = "y", formule = NULL),
    class = c("foret_aleatoire",
              if (classification) "classification" else "regression",
              "modele_arbre")
  )
}
