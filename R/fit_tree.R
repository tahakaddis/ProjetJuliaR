#' Ajuster un arbre de décision (classification ou régression)
#'
#' Classification si la cible est un facteur, régression si elle est numérique.
#'
#' @param data Un data frame.
#' @param target Nom de la variable cible.
#' @param ... Arguments passés à [rpart::rpart()].
#' @return Un modèle `rpart`.
#' @export
fit_tree <- function(data, target, ...) {
  f <- stats::as.formula(paste(target, "~ ."))
  method <- if (is.factor(data[[target]])) "class" else "anova"
  rpart::rpart(f, data = data, method = method, model = TRUE, ...)
}

#' Tracer un arbre de décision
#'
#' @param model Un modèle `rpart` issu de [fit_tree()].
#' @export
plot_tree <- function(model) {
  rpart.plot::rpart.plot(model)
}
