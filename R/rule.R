#' @title Validate data given a rule set
#'
#' Throws an error if the data does not confirm to the rule set
#'
#' @param data The data to validate
#' @param rule.file The yaml file containing the rules
#' @export
rule.validation <- function( data, rules.file )
{
  rules<-yaml::yaml.load_file(rules.file)

  for(rule in rules)
  {
    if(is.rule(rule))
      apply.rule(data,rule)
  }

}

apply.rule<-function(data,rule)
{
  results <- c()
  if("relation" %in% names(rule))
  {
    if (rule$relation == "greater_than" || rule$relation == ">")
      results <- data[[rule$field1]] > data[[rule$field2]]
    if (rule$relation == "lesser_than" || rule$relation == "<")
      results <- data[[rule$field1]] < data[[rule$field2]]
  }

  if (!all(results))
    warning("Rows: ", which(!results)[1], " fail rule: ", rule)
}

is.rule<-function(x)
{
  keywords=c("rule","relation","dependancy")
  return(any(keywords %in% names(x)))
}
