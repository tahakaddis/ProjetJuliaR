#' Importance des variables
#'
#' Fonction générique qui renvoie l'importance de chaque variable :
#' la diminution totale d'impureté (Gini ou SCR) apportée par les coupures
#' sur cette variable, exprimée en pourcentage. Pour une forêt, c'est la
#' moyenne sur tous les arbres.
#'
#' Une seule méthode, `importance.modele_arbre()`, sert à la fois aux arbres
#' et aux forêts : les deux classes héritent de `modele_arbre`.
#'
#' @param modele Un modèle issu de [arbre_decision()] ou [foret_aleatoire()].
#' @param ... Non utilisé.
#' @return Un vecteur nommé de classe `importance_variables`, trié par ordre
#'   décroissant, qui possède ses propres méthodes `print()` et `plot()`.
#' @export
importance <- function(modele, ...) {
  UseMethod("importance")
}

#' @rdname importance
#' @export
importance.modele_arbre <- function(modele, ...) {
  imp <- modele$importance
  if (sum(imp) > 0) imp <- 100 * imp / sum(imp)
  structure(sort(imp, decreasing = TRUE), class = "importance_variables")
}

#' @rdname importance
#' @param x Un objet de classe `importance_variables`.
#' @export
print.importance_variables <- function(x, ...) {
  cat("Importance des variables (% de la diminution d'impuret\u00e9)\n")
  print(round(unclass(x), 1))
  invisible(x)
}

#' @rdname importance
#' @param main Titre du graphique.
#' @export
plot.importance_variables <- function(x, main = "Importance des variables", ...) {
  valeurs <- rev(unclass(x))
  ancien <- graphics::par(mar = c(4, 8, 3, 1))
  on.exit(graphics::par(ancien))
  graphics::barplot(valeurs, horiz = TRUE, las = 1, col = "#4A7FB0",
                    border = NA, main = main, xlim = c(0, 1.1 * max(valeurs)),
                    xlab = "% de diminution d'impuret\u00e9")
  invisible(x)
}
