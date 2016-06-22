#' @title Validate data given a rule set
#'
#' Throws an error if the data does not confirm to the rule set
#'
#' @param data The data to validate
#' @param rules List of rules
#' @param rule_file Optional: the yaml file containing the rules
#' @export
rule_validation <- function( data, rules_file = NULL, rules = NULL )
{
  if (is.null(rules))
    rules<-yaml::yaml.load_file(rules_file)

  results <- c()
  for(rule in rules)
  {
    if(is_rule(rule))
      results <- c(results,apply_rule(data,rule))
  }

  return(all(results))
}

safe_is_na <- function(x)
{
  if (is.na(x))
    return(TRUE)
  if (is.character(x)) # is.character(x)&x=="NA" can fail, since R evaluates second even if first is F
    if (x=="NA")
      return(TRUE)
  return(FALSE)
}

relations <- list(
  list("keywords" = c("gt", "greater_than", ">"), "func"=function(x,y) x>y),
  list("keywords" = c("gte", "greater_than_or_equal", ">="), "func"=function(x,y) x>=y),
  list("keywords" = c("lt", "lesser_than", "<"), "func"=function(x,y) x<y),
  list("keywords" = c("lte", "lesser_than_or_equal", "<="), "func"=function(x,y) x<=y),
  list("keywords" = c("eq", "equal", "=="), "func"=function(x,y) {
    if (safe_is_na(x))
      return(safe_is_na(y))
    if (safe_is_na(y))
      return(safe_is_na(x))
    return(x==y)
  }),
  list("keywords" = c("not", "!="), "func"=function(x,y)
  {
    if (safe_is_na(x))
      return(!safe_is_na(y))
    if (safe_is_na(y))
      return(!safe_is_na(x))
    return(x!=y)
  })
)

get_conditional <- function( name )
{
  for( relation in relations )
     if (name %in% relation$keywords)
       return( relation$func )
  stop( "Operator ", name, " is not defined" )
}

apply_rule<-function(data,rule)
{
  results <- c()
  if("relation" %in% names(rule))
  {
    results <- get_conditional( rule$relation )( data[[rule$fields[1]]], data[[rule$fields[2]]] )
  }

  if("rule" %in% names(rule))
  {
    element<-strsplit(rule$rule," ")[[1]]
    new.rule<-list(fields=c(element[1],element[3]),relation=element[2])
    apply_rule(data,new.rule)
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

  if ("conditional" %in% names(rule))
  {
    op1 <- get_conditional( rule$conditional$operator[1] )
    op2 <- get_conditional( rule$conditional$operator[2] )

    fields1 <- data[[rule$fields[1]]]
    fields2 <- data[[rule$fields[2]]]

    results <- mapply( function( f1, f2 )
      { if (op1(f1,rule$conditional$values[1]))
          return(op2(f2,rule$conditional$values[2]))
        return(TRUE)
      },
      fields1, fields2 )
  }

  if (!all(results))
  {
    if("relation" %in% names(rule))
      warning.description<-paste(rule$fields[1], rule$relation, rule$fields[2])
    else
      warning.description<-rule
    warning("Rows: ", which(!results)[1], "; failed rule: ", warning.description)
    return(F)
  }
  return(T)
}

is_rule<-function(x)
{
  keywords=c("rule","relation","conditional","valid","invalid")
  return(any(keywords %in% names(x)))
}
