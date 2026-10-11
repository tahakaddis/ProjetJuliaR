#' Ajuster un arbre de décision (classification ou régression)
#'
#' Classification si la cible est un facteur, régression si elle est
#' numérique. Par défaut, l'arbre est élagué au paramètre de complexité
#' (`cp`) qui minimise l'erreur de validation croisée interne de rpart.
#'
#' @param data Un data frame.
#' @param target Nom de la variable cible.
#' @param prune Élaguer l'arbre ? (`TRUE` par défaut)
#' @param ... Arguments passés à [rpart::rpart()].
#' @return Un modèle `rpart`.
#' @export
fit_tree <- function(data, target, prune = TRUE, ...) {
  f <- stats::as.formula(paste(target, "~ ."))
  method <- if (is.factor(data[[target]])) "class" else "anova"
  model <- rpart::rpart(f, data = data, method = method, model = TRUE, ...)
  if (prune) {
    cp_opt <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
    model <- rpart::prune(model, cp = cp_opt)
  }
  model
}

#' Tracer un arbre de décision
#'
#' @param model Un modèle `rpart` issu de [fit_tree()].
#' @export
plot_tree <- function(model) {
  rpart.plot::rpart.plot(model)
}
