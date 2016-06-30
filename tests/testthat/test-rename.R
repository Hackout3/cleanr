context("rename")

test_that("empty", {
  d <- data.frame(a=1:10)
  expect_identical(rename(d, character()), d)
})

test_that("simple", {
  d <- data.frame(from1=1:10, from2=runif(10))
  cmp <- d
  names(cmp)[[1]] <- "to1"
  expect_equal(rename(d, c("from1"="to1")), cmp)
  expect_equal(rename(d, c("from1"="to1"), FALSE), cmp)
  expect_equal(rename(d, c("to1"="from1")), cmp)
  expect_equal(rename(d, c("to1"="from1"), TRUE), cmp)
})

test_that("yaml", {
  d <- data.frame(from1=1:10, from2=runif(10))
  res1 <- rename(d, "rename_table.yml")
  res2 <- rename(d, "rename_table.yml", FALSE)
  res3 <- rename(d, "rename_table_reverse.yml")
  res4 <- rename(d, "rename_table_reverse.yml", TRUE)
  expect_equal(res1, setNames(d, c("to1", "to2")))
  expect_equal(res1, res2)
  expect_equal(res1, res3)
  expect_equal(res1, res4)
})

test_that("duplicates", {
  d <- data.frame(from1=1:10, from2=runif(10))
  d2 <- rename(d, c(from1="to1", from1="to2"))
  d3 <- rename(d, c(to1="from1", to2="from1"), TRUE)
  expect_equal(names(d2), c("to1", "from2", "to2"))
  expect_identical(d2$to1, d2$to2)
  expect_equal(d3, d2)
})
