assert_named <- function(x, unique=TRUE, name=deparse(substitute(x))) {
  nms <- names(x)
  if (is.null(nms) || !all(nzchar(nms))) {
    stop(sprintf("%s must be named", name))
  }
  if (unique && any(duplicated(nms))) {
    stop(sprintf("Names of %s must be unique", name))
  }
}

assert_list <- function(x, name=deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", name), call.=FALSE)
  }
}
