context("rules")

test_that("Rules defined by relation keyword work", {
  data(epi.sim)
  expect_warning( rule.validation(epi.sim, rules=list(list(
    "field1"="date.of.onset", "field2"="date.of.hospitalisation", "relation"="gt" ) ) ))

   expect_true( rule.validation(epi.sim, rules=list(list(
    "field1"="date.of.onset", "field2"="date.of.hospitalisation", "relation"="lte" ) ) ))
})

test_that("We can exclude relations between fields", {

  data(epi.sim)
  rules <- list(list("fields"=c("gender","generation"), "values"=list("invalid"=c("f",0))))
  expect_warning( rule.validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "values"=list("invalid"=c("f",10))))
  expect_true( rule.validation(epi.sim, rules = rules ) )
})
