context("rules")

test_that("Rules defined by relation keyword work", {
  data(epi.sim)
  expect_warning( rule.validation(epi.sim, rules=list(list(
    "field1"="date.of.onset", "field2"="date.of.hospitalisation", "relation"="gt" ) ) ))

   expect_true( rule.validation(epi.sim, rules=list(list(
    "field1"="date.of.onset", "field2"="date.of.hospitalisation", "relation"="lte" ) ) ))
})
