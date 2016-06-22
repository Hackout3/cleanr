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

  results <- c()
  for(rule in rules)
  {
    if(is.rule(rule))
      results <- c(results,apply.rule(data,rule))
  }

  return(all(results))
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


  if("rule" %in% names(rule))
  {
    element<-strsplit(rule$rule," ")[[1]]
    new.rule<-list(field1=element[1],field2=element[3],relation=element[2])
    apply.rule(data,new.rule)
  }

  if("invalid" %in% names(rule))
  {
    values <- rule$invalid
    # Support both [value1, value2] and [[value1, value2],[alt_value1,alt_value2]], so we normalize them here
    if (class(values)!="list")
    {
      values <- list(values)
    }

    for (value in values)
    {
      results.df <- mapply(function(f,v) (data[[f]]==v), rule$fields, value)
      results <- !(rowSums(results.df)==ncol(results.df)) # If all are true then all fields match all values (and sum==ncol)
    }
  }

  if("valid" %in% names(rule))
  {
    values <- rule$valid
    # Support both [value1, value2] and [[value1, value2],[alt_value1,alt_value2]], so we normalize them here
    if (class(values)!="list")
    {
      values <- list(values)
    }

    results <- NULL
    for (value in values)
    {
      results.df <- mapply(function(f,v) (data[[f]]==v), rule$fields, value)
      if (is.null(results))
        results <- (rowSums(results.df)==ncol(results.df))
      else
        results <- (rowSums(results.df)==ncol(results.df))|results # Match this set of values or the previous set of values
    }
  }

  if (!all(results))
  {
    warning("Rows: ", which(!results)[1], " fail rule: ", rule)
    return(F)
  }
  return(T)
}

is.rule<-function(x)
{
  keywords=c("rule","relation","dependancy","valid","invalid")
  return(any(keywords %in% names(x)))
}
