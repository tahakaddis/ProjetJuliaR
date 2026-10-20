coimbra <- read.csv("data-raw/dataR2.csv")
names(coimbra)[names(coimbra) == "MCP.1"] <- "MCP1"

# --- Classification : sain / cancer ---
coimbra$Classification <- factor(coimbra$Classification,
                                 levels = c(1, 2),
                                 labels = c("sain", "cancer"))

# --- Régression : log(Leptine) ---
# HOMA exclu car calculé à partir de Glucose et Insulin
coimbra_reg <- coimbra[, c("Age", "BMI", "Glucose", "Insulin",
                           "Adiponectin", "Resistin", "MCP1")]
coimbra_reg$logLeptin <- log(coimbra$Leptin)

# --- Plis de validation croisée communs R / Julia ---
set.seed(123)
folds <- sample(rep(1:5, length.out = nrow(coimbra)))

# Fichiers pour la partie Julia
write.csv(coimbra,     "data-raw/coimbra_clean.csv", row.names = FALSE)
write.csv(coimbra_reg, "data-raw/coimbra_reg.csv",   row.names = FALSE)
write.csv(data.frame(fold = folds), "data-raw/folds.csv", row.names = FALSE)

usethis::use_data(coimbra, coimbra_reg, overwrite = TRUE)
