test_that("fit_tree works on iris", {
  s <- split_data(iris)
  tree <- fit_tree(s$train, "Species")
  res <- evaluate_model(tree, s$test, "Species")
  expect_s3_class(tree, "rpart")
  expect_true(res$accuracy > 0.8)
})
