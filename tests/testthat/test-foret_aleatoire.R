test_that("la forêt de classification fonctionne", {
  set.seed(1)
  f <- foret_aleatoire(Classification ~ ., data = coimbra, ntree = 50)
  expect_s3_class(f, c("foret_aleatoire", "classification", "modele_arbre"))
  expect_length(f$arbres, 50)
  expect_true(f$erreur_oob >= 0 && f$erreur_oob <= 1)
  proba <- predict(f, coimbra, type = "proba")
  expect_true(all(proba >= 0 & proba <= 1))
})

test_that("la forêt de régression fonctionne", {
  set.seed(1)
  f <- foret_aleatoire(logLeptin ~ ., data = coimbra_reg, ntree = 50)
  expect_s3_class(f, "regression")
  expect_gt(evaluer(f, coimbra_reg)$r2, 0.5)
})

test_that("importance() fonctionne pour un arbre et une forêt", {
  set.seed(1)
  imp_a <- importance(arbre_decision(Classification ~ ., data = coimbra))
  imp_f <- importance(foret_aleatoire(Classification ~ ., data = coimbra, ntree = 20))
  expect_s3_class(imp_a, "importance_variables")
  expect_equal(sum(imp_f), 100)
})
