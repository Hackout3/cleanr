context("rename")

test_that("empty", {
  d <- data.frame(a=1:10)
  expect_identical(rename(d, character()), d)
})

test_that("yaml", {
  d <- data.frame(from1=1:10, from2=runif(10))
  rename(d, "rename_table.yml")
})

test_that("duplicates", {
  d <- data.frame(from1=1:10, from2=runif(10))
  d2 <- rename(d, c(to1="from1", to2="from1"))
  expect_equal(names(d2), c("to1", "from2", "to2"))
  expect_identical(d2$to1, d2$to2)
})
