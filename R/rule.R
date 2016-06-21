#' @title Validate data given a rule set
#'
#' Throws an error if the data does not confirm to the rule set
#'
#' @param data The data to validate
#' @param rules List of rules
#' @param rule.file Optional: the yaml file containing the rules
#' @export
rule.validation <- function( data, rules.file = NULL, rules = NULL )
{
  if (is.null(rules))
    rules<-yaml::yaml.load_file(rules.file)

  for(rule in rules)
  {
    if(is.rule(rule))
      apply.rule(data,rule)
  }

}

relations <- list(
  list("keywords" = c("gt", "greater_than", ">"), "func"=function(x,y) x>y),
  list("keywords" = c("gte", "greater_than_or_equal", ">="), "func"=function(x,y) x>=y),
  list("keywords" = c("lt", "lesser_than", "<"), "func"=function(x,y) x<y),
  list("keywords" = c("lte", "lesser_than_or_equal", "<="), "func"=function(x,y) x<=y)
)

apply.rule<-function(data,rule)
{
  results <- c()
  if("relation" %in% names(rule))
  {
    for( relation in relations )
      if (rule$relation %in% relation$keywords)
      {
        results <- relation$func( data[[rule$field1]], data[[rule$field2]] )
        break
      }
  }

  if (!all(results))
    warning("Rows: ", which(!results)[1], " fail rule: ", rule)
}

is.rule<-function(x)
{
  keywords=c("rule","relation","dependancy")
  return(any(keywords %in% names(x)))
}
