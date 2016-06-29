context("recode")

test_that("recode two columns", {
  rules <- recode_read("two.yml")
  data <- data.frame(old1=runif(10), old2=runif(10))
  data2 <- recode(data, rules)
  expect_true("min" %in% names(data2))
  expect_equal(data2$min, pmin(data$old1, data$old2))
})
