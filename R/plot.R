#' Dessiner un arbre de décision
#'
#' Méthode de la fonction générique [plot()] pour la classe
#' `arbre_decision`. Le dessin est entièrement réalisé avec les fonctions de
#' base de R (`graphics`) : les feuilles sont placées de gauche à droite, et
#' chaque nœud interne est centré au-dessus de ses deux enfants.
#'
#' @param x Un arbre issu de [arbre_decision()].
#' @param chiffres Nombre de chiffres significatifs des seuils et prédictions.
#' @param cex Taille du texte.
#' @param ... Non utilisé.
#' @return `x`, de façon invisible.
#' @export
plot.arbre_decision <- function(x, chiffres = 3, cex = 0.8, ...) {
  noeuds <- positionner_noeuds(x, chiffres)
  n_feuilles <- sum(noeuds$feuille)
  prof <- max(-noeuds$y)

  ancien <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(graphics::par(ancien))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0.4, n_feuilles + 0.6),
                        ylim = c(-prof - 0.5, 0.4))

  # Branches : gauche = condition vraie ("oui"), droite = "non".
  enfants <- noeuds[!is.na(noeuds$parent), ]
  parents <- noeuds[match(enfants$parent, noeuds$id), ]
  graphics::segments(parents$x, parents$y, enfants$x, enfants$y,
                     col = "grey50")
  graphics::text((parents$x + enfants$x) / 2, (parents$y + enfants$y) / 2,
                 ifelse(enfants$id %% 2 == 0, "oui", "non"),
                 cex = cex * 0.8, col = "grey40", font = 3)

  # Boîtes : couleur selon la classe prédite (classification) ou gris.
  if (x$type == "classification") {
    palette <- grDevices::hcl.colors(length(x$niveaux), "Set 2")
    fond <- ifelse(noeuds$feuille, palette[noeuds$prediction], "grey95")
  } else {
    fond <- ifelse(noeuds$feuille, "#CFE3F3", "grey95")
  }
  largeur <- graphics::strwidth(noeuds$label, cex = cex) / 2 + 0.06
  hauteur <- graphics::strheight(noeuds$label, cex = cex) / 2 + 0.06
  graphics::rect(noeuds$x - largeur, noeuds$y - hauteur,
                 noeuds$x + largeur, noeuds$y + hauteur,
                 col = fond, border = "grey40")
  graphics::text(noeuds$x, noeuds$y, noeuds$label, cex = cex)
  invisible(x)
}

# Calcule la position (x, y) et l'étiquette de chaque nœud.
positionner_noeuds <- function(modele, chiffres) {
  lignes <- list()
  compteur <- 0

  visiter <- function(noeud, id, parent, profondeur) {
    if (noeud$feuille) {
      compteur <<- compteur + 1
      x <- compteur
    } else {
      xg <- visiter(noeud$gauche, 2 * id, id, profondeur + 1)
      xd <- visiter(noeud$droite, 2 * id + 1, id, profondeur + 1)
      x <- (xg + xd) / 2
    }
    if (noeud$feuille) {
      if (modele$type == "classification") {
        label <- sprintf("%s\n%.0f %%\nn = %d", modele$niveaux[noeud$prediction],
                         100 * max(noeud$proba), noeud$n)
      } else {
        label <- sprintf("%s\nn = %d", format(noeud$prediction, digits = chiffres),
                         noeud$n)
      }
    } else {
      label <- paste(noeud$variable, "<", format(noeud$seuil, digits = chiffres))
    }
    lignes[[length(lignes) + 1]] <<- data.frame(
      id = id, parent = parent, x = x, y = -profondeur, label = label,
      feuille = noeud$feuille,
      prediction = if (modele$type == "classification") noeud$prediction else NA
    )
    x
  }

  visiter(modele$racine, 1, NA, 0)
  do.call(rbind, lignes)
}
