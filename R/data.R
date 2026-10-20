#' Cancer du sein - Coimbra (classification)
#'
#' 116 patientes : 9 marqueurs issus d'une prise de sang et le diagnostic.
#'
#' @format Un data frame de 116 lignes et 10 colonnes : `Age`, `BMI`,
#'   `Glucose`, `Insulin`, `HOMA`, `Leptin`, `Adiponectin`, `Resistin`,
#'   `MCP1`, et `Classification` ("sain" / "cancer").
#' @source Patrício et al. (2018), UCI Machine Learning Repository,
#'   <https://archive.ics.uci.edu/dataset/451>.
"coimbra"

#' Cancer du sein - Coimbra (régression)
#'
#' Sous-ensemble de `coimbra` pour prédire le taux de leptine,
#' une hormone produite par le tissu adipeux.
#'
#' @format Un data frame de 116 lignes et 8 colonnes : `Age`, `BMI`,
#'   `Glucose`, `Insulin`, `Adiponectin`, `Resistin`, `MCP1`, et
#'   `logLeptin` (log du taux de leptine).
#' @source Patrício et al. (2018), UCI Machine Learning Repository,
#'   <https://archive.ics.uci.edu/dataset/451>.
"coimbra_reg"
