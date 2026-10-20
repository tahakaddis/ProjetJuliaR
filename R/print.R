#' Affichage des modèles
#'
#' Méthodes de la fonction générique [print()].
#'
#' Elles illustrent l'**héritage** S3 : `print.arbre_decision()` et
#' `print.foret_aleatoire()` affichent ce qui est propre à leur classe, puis
#' appellent `NextMethod()`, qui exécute `print.modele_arbre()`, la méthode
#' de la classe parente commune.
#'
#' @param x Un modèle issu de [arbre_decision()] ou [foret_aleatoire()].
#' @param chiffres Nombre de chiffres significatifs affichés.
#' @param ... Non utilisé.
#' @return `x`, de façon invisible.
#' @name print_modele
NULL

#' @rdname print_modele
#' @export
print.modele_arbre <- function(x, ...) {
  cat("  Type        :", x$type, "\n")
  cat("  Cible       :", x$cible, "\n")
  cat("  Observations:", x$n, "\n")
  cat("  Variables   :", paste(x$variables, collapse = ", "), "\n")
  invisible(x)
}

#' @rdname print_modele
#' @export
print.arbre_decision <- function(x, chiffres = 3, ...) {
  cat("Arbre de d\u00e9cision (CART)\n")
  NextMethod()
  cat("  Feuilles    :", compter_feuilles(x$racine),
      "| profondeur :", profondeur_arbre(x$racine), "\n\n")
  cat("noeud) condition  n  pr\u00e9diction\n")
  cat("      * = feuille\n\n")
  afficher_noeud(x$racine, id = 1, condition = "racine", niveau = 0,
                 modele = x, chiffres = chiffres)
  invisible(x)
}

#' @rdname print_modele
#' @export
print.foret_aleatoire <- function(x, ...) {
  cat("For\u00eat al\u00e9atoire\n")
  NextMethod()
  cat("  Arbres      :", x$ntree, "\n")
  cat("  mtry        :", x$parametres$mtry, "\n")
  if (x$type == "classification") {
    cat("  Erreur OOB  :", sprintf("%.1f %%", 100 * x$erreur_oob), "\n")
  } else {
    cat("  MSE OOB     :", format(x$erreur_oob, digits = 4), "\n")
  }
  invisible(x)
}

# Affichage récursif d'un nœud, dans le style de rpart.
afficher_noeud <- function(noeud, id, condition, niveau, modele, chiffres) {
  if (modele$type == "classification") {
    pred <- sprintf("%s (%s)", modele$niveaux[noeud$prediction],
                    paste(format(noeud$proba, digits = chiffres), collapse = " "))
  } else {
    pred <- format(noeud$prediction, digits = chiffres)
  }
  cat(strrep("  ", niveau), id, ") ", condition, "  ", noeud$n, "  ", pred,
      if (noeud$feuille) " *", "\n", sep = "")
  if (!noeud$feuille) {
    seuil <- format(noeud$seuil, digits = chiffres)
    afficher_noeud(noeud$gauche, 2 * id, paste(noeud$variable, "<", seuil),
                   niveau + 1, modele, chiffres)
    afficher_noeud(noeud$droite, 2 * id + 1, paste(noeud$variable, ">=", seuil),
                   niveau + 1, modele, chiffres)
  }
}
