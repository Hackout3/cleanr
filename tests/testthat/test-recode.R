context("recode")

test_that("rename", {
  rules <- recode_read("rename.yml")
  data <- data.frame(old=runif(10))
  data2 <- recode(data, rules)
  expect_identical(data2$new, sqrt(data$old))
})

test_that("recipe", {
  rules <- recode_read("use_recipe.yml", "rename_recipes.yml")
  data <- data.frame(old=as.character(runif(10)), stringsAsFactors=FALSE)
  data2 <- recode(data, rules)
  expect_equal(data2$old, sqrt(as.numeric(data$old)))
  expect_equal(names(data2), names(data))
})

test_that("recipe rename", {
  rules <- recode_read("rename_recipe.yml", "rename_recipes.yml")
  expect_equal(rules$new, rules$equiv)

  data <- data.frame(old=as.character(runif(10)), stringsAsFactors=FALSE)
  data2 <- recode(data, rules)

  expect_equal(data2$old, sqrt(as.numeric(data$old)))
  expect_equal(data2$new, sqrt(as.numeric(data$old)))
  expect_equal(data2$equiv, sqrt(as.numeric(data$old)))
})

test_that("recode two columns", {
  rules <- recode_read("two.yml")
  data <- data.frame(old1=runif(10), old2=runif(10))
  data2 <- recode(data, rules)
  expect_true("min" %in% names(data2))
  expect_equal(data2$min, pmin(data$old1, data$old2))
})
