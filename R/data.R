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
#' Sous-ensemble de [coimbra] pour prédire la résistance à l'insuline.
#' `Glucose` et `Insulin` sont exclus car HOMA en est calculé directement.
#'
#' @format Un data frame de 116 lignes et 7 colonnes : `Age`, `BMI`,
#'   `Leptin`, `Adiponectin`, `Resistin`, `MCP1`, et `logHOMA` (log de HOMA).
#' @source Patrício et al. (2018), UCI Machine Learning Repository,
#'   <https://archive.ics.uci.edu/dataset/451>.
"coimbra_reg"
