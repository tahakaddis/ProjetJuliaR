coimbra <- read.csv("data-raw/dataR2.csv")
names(coimbra)[names(coimbra) == "MCP.1"] <- "MCP1"

# --- Classification : sain / cancer ---
coimbra$Classification <- factor(coimbra$Classification,
                                 levels = c(1, 2),
                                 labels = c("sain", "cancer"))

# --- Régression : log(HOMA) ---
# Glucose et Insulin exclus car HOMA = Glucose * Insulin / 405
coimbra_reg <- coimbra[, c("Age", "BMI", "Leptin", "Adiponectin",
                           "Resistin", "MCP1")]
coimbra_reg$logHOMA <- log(coimbra$HOMA)

# Fichiers pour la partie Julia
write.csv(coimbra,     "data-raw/coimbra_clean.csv", row.names = FALSE)
write.csv(coimbra_reg, "data-raw/coimbra_reg.csv",   row.names = FALSE)

usethis::use_data(coimbra, coimbra_reg, overwrite = TRUE)
