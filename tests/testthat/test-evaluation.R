test_that("evaluer dispatche selon le type de tâche", {
  a_c <- arbre_decision(Classification ~ ., data = coimbra)
  a_r <- arbre_decision(logLeptin ~ ., data = coimbra_reg)
  expect_s3_class(evaluer(a_c, coimbra), "metriques_classification")
  expect_s3_class(evaluer(a_r, coimbra_reg), "metriques_regression")
  expect_named(as.data.frame(evaluer(a_c, coimbra)),
               c("accuracy", "sensibilite", "specificite", "auc"))
})

test_that("l'AUC vaut 1 pour un classement parfait et 0.5 au hasard", {
  expect_equal(auc_mann_whitney(c(0.1, 0.2, 0.8, 0.9), c(FALSE, FALSE, TRUE, TRUE)), 1)
  expect_equal(auc_mann_whitney(c(0.5, 0.5, 0.5, 0.5), c(FALSE, FALSE, TRUE, TRUE)), 0.5)
})

test_that("creer_plis reproduit folds.csv", {
  plis <- creer_plis(116)
  expect_equal(as.vector(table(plis)), c(24, 23, 23, 23, 23))
})

test_that("validation_croisee renvoie une ligne par pli", {
  cv <- validation_croisee(Classification ~ ., coimbra, arbre_decision,
                           creer_plis(nrow(coimbra)))
  expect_s3_class(cv, "resultat_cv")
  expect_equal(nrow(cv), 5)
})
