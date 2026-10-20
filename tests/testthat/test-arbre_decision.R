test_that("arbre_decision dispatche sur formula et default", {
  a1 <- arbre_decision(Species ~ ., data = iris)
  a2 <- arbre_decision(iris[, 1:4], iris$Species)
  expect_s3_class(a1, c("arbre_decision", "classification", "modele_arbre"))
  expect_identical(predict(a1, iris), predict(a2, iris))
})

test_that("l'arbre de classification apprend bien iris", {
  a <- arbre_decision(Species ~ ., data = iris)
  expect_gt(evaluer(a, iris)$accuracy, 0.9)
  proba <- predict(a, iris, type = "proba")
  expect_equal(dim(proba), c(150, 3))
  expect_equal(unname(rowSums(proba)), rep(1, 150))
})

test_that("l'arbre de régression renvoie des valeurs numériques", {
  a <- arbre_decision(logLeptin ~ ., data = coimbra_reg)
  expect_s3_class(a, "regression")
  pred <- predict(a, coimbra_reg)
  expect_type(pred, "double")
  expect_length(pred, nrow(coimbra_reg))
})

test_that("la première coupure est la même que celle de rpart", {
  skip_if_not_installed("rpart")
  a <- arbre_decision(Classification ~ ., data = coimbra)
  r <- rpart::rpart(Classification ~ ., data = coimbra)
  expect_equal(a$racine$variable, as.character(r$frame$var[1]))
  expect_equal(a$racine$seuil, unname(r$splits[1, "index"]))
})

test_that("profondeur_max limite bien l'arbre", {
  a <- arbre_decision(Classification ~ ., data = coimbra, profondeur_max = 1)
  expect_equal(compter_feuilles(a$racine), 2L)
})

test_that("les prédicteurs non numériques sont refusés", {
  d <- data.frame(x = c("a", "b", "c"), y = factor(c("u", "v", "u")))
  expect_error(arbre_decision(y ~ ., data = d), "num")
})
