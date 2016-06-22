context("rules")

test_that("Rules defined by relation keyword work", {
  data(epi.sim)
  expect_warning( rule.validation(epi.sim, rules=list(list(
    "fields"=c("date.of.onset", "date.of.hospitalisation"), "relation"="gt" ) ) ))

   expect_true( rule.validation(epi.sim, rules=list(list(
    "fields"=c("date.of.onset", "date.of.hospitalisation"), "relation"="lte" ) ) ))
})

test_that("We can exclude value combinations for given fields", {
  data(epi.sim)
  rules <- list(list("fields"=c("gender","generation"), "invalid"=c("f",0)))
  expect_warning( rule.validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "invalid"=c("f",100)))
  expect_true( rule.validation(epi.sim, rules = rules ) )
})

test_that("We can require value combinations for given fields", {
  data(epi.sim)
  epi.sim <- head(epi.sim,2)
  rules <- list(list("fields"=c("gender","generation"), "valid"=c("f",0)))
  expect_warning( rule.validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "valid"=list(c("f",0),c("m",1))))
  expect_true( rule.validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "valid"=list(c("f",0),c("m",0))))
  expect_warning( rule.validation(epi.sim, rules = rules ) )
})
