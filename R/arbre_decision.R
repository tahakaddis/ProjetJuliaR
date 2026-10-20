#' Arbre de décision (algorithme CART)
#'
#' Construit un arbre de décision binaire selon l'algorithme CART,
#' (sans `rpart`).
#'
#' * Si la cible est un **facteur**, l'arbre fait de la **classification** :
#'   chaque coupure minimise l'impureté de Gini des deux nœuds enfants.
#' * Si la cible est **numérique**, l'arbre fait de la **régression** :
#'   chaque coupure minimise la somme des carrés des résidus (SCR).
#'
#' `arbre_decision()` est une fonction **générique** : la méthode exécutée
#' dépend de la classe du premier argument.
#' * `arbre_decision(formule, data)` appelle `arbre_decision.formula()` ;
#' * `arbre_decision(x, y)` appelle `arbre_decision.default()`.
#'
#' @param x Une formule (`cible ~ .`) ou un data frame / une matrice de
#'   prédicteurs numériques.
#' @param ... Arguments passés aux méthodes.
#' @return Un objet S3 de classe
#'   `c("arbre_decision", "classification" | "regression", "modele_arbre")`.
#' @seealso [predict.arbre_decision()], [plot.arbre_decision()],
#'   [foret_aleatoire()]
#' @examples
#' arbre <- arbre_decision(Classification ~ ., data = coimbra)
#' arbre
#' plot(arbre)
#' @export
arbre_decision <- function(x, ...) {
  UseMethod("arbre_decision")
}

#' @rdname arbre_decision
#' @param data Data frame contenant les variables de la formule.
#' @export
arbre_decision.formula <- function(x, data, ...) {
  mf <- stats::model.frame(x, data)
  modele <- arbre_decision.default(mf[, -1, drop = FALSE],
                                   stats::model.response(mf), ...)
  modele$formule <- x
  modele$cible <- names(mf)[1]
  modele
}

#' @rdname arbre_decision
#' @param y Variable cible : facteur (classification) ou numérique
#'   (régression).
#' @param profondeur_max Profondeur maximale de l'arbre (la racine est à la
#'   profondeur 0).
#' @param min_noeud Nombre minimal d'observations dans un nœud pour essayer
#'   de le couper.
#' @param min_feuille Nombre minimal d'observations dans chaque feuille.
#' @param cp Paramètre de complexité : une coupure n'est gardée que si elle
#'   réduit l'impureté d'au moins `cp` fois l'impureté de la racine.
#' @param mtry Nombre de variables tirées au hasard à chaque nœud.
#'   `NULL` (défaut) = toutes les variables. Utilisé par [foret_aleatoire()].
#' @export
arbre_decision.default <- function(x, y, profondeur_max = 30, min_noeud = 20,
                                   min_feuille = round(min_noeud / 3),
                                   cp = 0.01, mtry = NULL, ...) {
  x <- verifier_predicteurs(x)
  if (length(y) != nrow(x)) {
    stop("`x` et `y` doivent avoir le m\u00eame nombre d'observations.")
  }
  if (anyNA(y)) stop("La cible contient des valeurs manquantes.")
  if (is.null(mtry)) mtry <- ncol(x)
  parametres <- list(profondeur_max = profondeur_max, min_noeud = min_noeud,
                     min_feuille = min_feuille, cp = cp,
                     mtry = min(mtry, ncol(x)))
  ajuster_arbre(x, y, parametres)
}

# --------------------------------------------------------------------------
# Constructeur bas niveau : crée l'objet S3 à partir de ses composants.
# --------------------------------------------------------------------------
new_arbre_decision <- function(racine, type, niveaux, variables, parametres,
                               importance, n) {
  structure(
    list(racine = racine, type = type, niveaux = niveaux,
         variables = variables, parametres = parametres,
         importance = importance, n = n, cible = "y", formule = NULL),
    class = c("arbre_decision", type, "modele_arbre")
  )
}

# --------------------------------------------------------------------------
# Ajustement : x est une matrice numérique déjà vérifiée.
# --------------------------------------------------------------------------
ajuster_arbre <- function(x, y, parametres) {
  if (is.factor(y)) {
    type <- "classification"
    niveaux <- levels(y)
    y <- as.integer(y)
  } else if (is.numeric(y)) {
    type <- "regression"
    niveaux <- NULL
  } else {
    stop("La cible doit \u00eatre un facteur (classification) ou num\u00e9rique (r\u00e9gression).")
  }

  # Le contexte est un environnement : il est partagé (et modifiable) par
  # tous les appels récursifs, ce qui permet d'accumuler l'importance.
  ctx <- new.env()
  ctx$type <- type
  ctx$K <- length(niveaux)
  ctx$p <- parametres
  ctx$seuil_gain <- parametres$cp * impurete(y, type, ctx$K)
  ctx$importance <- stats::setNames(numeric(ncol(x)), colnames(x))

  racine <- construire_noeud(x, y, profondeur = 0, ctx = ctx)
  new_arbre_decision(racine, type, niveaux, colnames(x), parametres,
                     ctx$importance, nrow(x))
}

# Impureté totale d'un nœud :
#  - classification : n * Gini = n * (1 - somme des p_k^2)
#  - régression     : somme des carrés des écarts à la moyenne
impurete <- function(y, type, K) {
  n <- length(y)
  if (type == "classification") {
    effectifs <- tabulate(y, K)
    n - sum(effectifs^2) / n
  } else {
    sum((y - mean(y))^2)
  }
}

# Feuille : ce que le nœud prédirait s'il n'était pas coupé.
creer_feuille <- function(y, type, K) {
  if (type == "classification") {
    effectifs <- tabulate(y, K)
    list(feuille = TRUE, n = length(y), prediction = which.max(effectifs),
         proba = effectifs / length(y))
  } else {
    list(feuille = TRUE, n = length(y), prediction = mean(y))
  }
}

# Construction récursive de l'arbre.
construire_noeud <- function(x, y, profondeur, ctx) {
  noeud <- creer_feuille(y, ctx$type, ctx$K)
  n <- length(y)
  pur <- if (ctx$type == "classification") max(noeud$proba) == 1 else all(y == y[1])

  if (profondeur >= ctx$p$profondeur_max || n < ctx$p$min_noeud || pur) {
    return(noeud)
  }

  variables <- if (ctx$p$mtry < ncol(x)) sample.int(ncol(x), ctx$p$mtry) else seq_len(ncol(x))
  coupure <- meilleure_coupure(x, y, variables, ctx)
  if (is.null(coupure) || coupure$gain <= 0 || coupure$gain < ctx$seuil_gain) {
    return(noeud)
  }

  j <- coupure$variable
  ctx$importance[j] <- ctx$importance[j] + coupure$gain
  a_gauche <- x[, j] < coupure$seuil

  noeud$feuille <- FALSE
  noeud$variable <- colnames(x)[j]
  noeud$seuil <- coupure$seuil
  noeud$gain <- coupure$gain
  noeud$gauche <- construire_noeud(x[a_gauche, , drop = FALSE], y[a_gauche],
                                   profondeur + 1, ctx)
  noeud$droite <- construire_noeud(x[!a_gauche, , drop = FALSE], y[!a_gauche],
                                   profondeur + 1, ctx)
  noeud
}

# Recherche de la meilleure coupure "x_j < seuil" parmi les variables données.
# Pour chaque variable, on trie les observations puis on évalue TOUS les
# seuils possibles d'un coup grâce à des sommes cumulées (complexité n log n).
meilleure_coupure <- function(x, y, variables, ctx) {
  n <- length(y)
  parent <- impurete(y, ctx$type, ctx$K)
  n_gauche <- seq_len(n - 1)
  n_droite <- n - n_gauche
  meilleure <- NULL

  for (j in variables) {
    ordre <- order(x[, j])
    xs <- unname(x[ordre, j])
    ys <- y[ordre]
    if (xs[1] == xs[n]) next   # variable constante dans ce nœud

    if (ctx$type == "classification") {
      # effectifs cumulés de chaque classe à gauche du seuil
      cum <- vapply(seq_len(ctx$K), function(k) cumsum(ys == k), numeric(n))
      cum <- matrix(cum, nrow = n)
      gauche <- cum[-n, , drop = FALSE]
      droite <- matrix(cum[n, ], n - 1, ctx$K, byrow = TRUE) - gauche
      imp_gauche <- n_gauche - rowSums(gauche^2) / n_gauche
      imp_droite <- n_droite - rowSums(droite^2) / n_droite
    } else {
      s <- cumsum(ys)
      s2 <- cumsum(ys^2)
      s_g <- s[-n]
      s2_g <- s2[-n]
      imp_gauche <- s2_g - s_g^2 / n_gauche
      imp_droite <- (s2[n] - s2_g) - (s[n] - s_g)^2 / n_droite
    }

    enfants <- imp_gauche + imp_droite
    valide <- xs[-n] < xs[-1] &
      n_gauche >= ctx$p$min_feuille & n_droite >= ctx$p$min_feuille
    if (!any(valide)) next
    enfants[!valide] <- Inf
    i <- which.min(enfants)
    gain <- unname(parent - enfants[i])

    if (is.null(meilleure) || gain > meilleure$gain) {
      meilleure <- list(variable = j, seuil = (xs[i] + xs[i + 1]) / 2,
                        gain = gain)
    }
  }
  meilleure
}

# Vérifie et convertit les prédicteurs en matrice numérique.
verifier_predicteurs <- function(x) {
  x <- as.data.frame(x)
  numeriques <- vapply(x, is.numeric, logical(1))
  if (!all(numeriques)) {
    stop("Toutes les variables explicatives doivent \u00eatre num\u00e9riques : ",
         paste(names(x)[!numeriques], collapse = ", "))
  }
  if (anyNA(x)) stop("Les variables explicatives contiennent des valeurs manquantes.")
  as.matrix(x)
}

# Nombre de feuilles et profondeur (utiles pour print et plot).
compter_feuilles <- function(noeud) {
  if (noeud$feuille) 1L else compter_feuilles(noeud$gauche) + compter_feuilles(noeud$droite)
}

profondeur_arbre <- function(noeud) {
  if (noeud$feuille) 0L else 1L + max(profondeur_arbre(noeud$gauche),
                                      profondeur_arbre(noeud$droite))
}
