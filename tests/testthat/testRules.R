context("rules")

test_that("Rules defined by relation keyword work", {
  data(epi.sim)
  expect_warning( rule_validation(epi.sim, rules=list(list(
    "fields"=c("date.of.onset", "date.of.hospitalisation"), "relation"="gt" ) ) ))

   expect_true( rule_validation(epi.sim, rules=list(list(
    "fields"=c("date.of.onset", "date.of.hospitalisation"), "relation"="lte" ) ) ))
})

test_that("We can exclude value combinations for given fields", {
  data(epi.sim)
  rules <- list(list("fields"=c("gender","generation"), "invalid"=c("f",0)))
  expect_warning( rule_validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "invalid"=c("f",100)))
  expect_true( rule_validation(epi.sim, rules = rules ) )
})

test_that("We can require value combinations for given fields", {
  data(epi.sim)
  epi.sim <- head(epi.sim,2)
  rules <- list(list("fields"=c("gender","generation"), "valid"=c("f",0)))
  expect_warning( rule_validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "valid"=list(c("f",0),c("m",1))))
  expect_true( rule_validation(epi.sim, rules = rules ) )

  rules <- list(list("fields"=c("gender","generation"), "valid"=list(c("f",0),c("m",0))))
  expect_warning( rule_validation(epi.sim, rules = rules ) )
})

test_that("We can get a conditional function by name", {
  expect_true( get_conditional("==")( 1,1 ) )
  expect_false( get_conditional("==")( 1,2 ) )
  expect_true( get_conditional("<=")( 1,2 ) )
  expect_true( get_conditional("<=")( 1,1 ) )

  # NA values
  expect_true( get_conditional("==")( "NA",NA ) )
  expect_true( get_conditional("==")( NA,"NA" ) )
  expect_true( get_conditional("==")( NA,NA ) )
  expect_true( get_conditional("==")( "NA","NA" ) )

  expect_true( get_conditional("!=")( "NA",1 ) )
  expect_true( get_conditional("!=")( NA,1 ) )
  expect_false( get_conditional("!=")( "NA",NA ) )
  expect_false( get_conditional("!=")( NA,"NA" ) )
  expect_false( get_conditional("!=")( NA,NA ) )
  expect_false( get_conditional("!=")( "NA","NA" ) )
})

test_that("We can specify conditionals", {
  data(epi.sim)

  rules <- list(list("fields"=c("date.of.death","outcome"),
              "conditional"=list("operator" = c("not","equal"), "values"=c("NA","Death"))))
  expect_true( rule_validation(epi.sim[c(4,10),], rules = rules ) )

  rules <- list(list("fields"=c("date.of.death","outcome"),
              "conditional"=list("operator" = c("not","equal"), "values"=c("NA","Death"))))
  expect_warning( rule_validation(epi.sim, rules = rules ) )
})
