## Read the recoding yaml.

recode_read <- function(filename) {
  dat <- yaml_read(filename)
  recode_validate(dat)

  prepare_multi_rule <- function(x) {
    if (is.null(names(x))) {
      unlist(x, FALSE)
    } else {
      x
    }
  }
  dat <- unlist(dat, FALSE)
  lapply(dat, prepare_multi_rule)
}

recode_validate <- function(dat) {
  ## OK, in the absence of a yaml schema, we're going to have to faff
  ## here a bit:
  ## stopifnot(is.null(dat))


  ## assert_named(dat)
  ## lapply(dat, recode_validate1)
}

recode_validate1 <- function(x, top=TRUE) {
  if (is.list(x) && is.null(names(x))) {
    if (top) {
      lapply(x, recode_validate1)
    } else {
      stop("I am confused")
    }
  }
  assert_named(x, FALSE)
  ## TODO: validate arguments against functions and check functions exist.
}


assert_named <- function(x, unique=TRUE, name=deparse(substitute(x))) {
  nms <- names(x)
  if (is.null(nms) || !all(nzchar(nms))) {
    stop(sprintf("%s must be named", name))
  }
  if (unique && any(duplicated(nms))) {
    stop(sprintf("Names of %s must be unique", name))
  }
}
