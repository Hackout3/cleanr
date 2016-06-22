## TODO: export and document all of these.

recode_character <- function(x, table, unmatched_action="missing") {
  ## TODO: Do this validation elsewhere?
  if (is.list(table)) {
    stopifnot(all(lengths(table) == 1))
    table <- unlist(table)
  }
  unmatched_action <- match.arg(unmatched_action, c("missing", "error", "keep"))
  if (is.null(names(table))) {
    from <- seq_along(table)
    to <- table
  } else {
    from <- names(table)
    to <- unname(table)
  }
  i <- match(x, from)
  j <- is.na(i) & !is.na(x)
  ret <- setNames(to[i], names(x))
  if (any(j)) {
    if (unmatched_action == "error") {
      stop("Unknown category: ",
           paste(unique(x[j]), collapse=", "))
    } else if (unmatched_action == "keep") {
      ret[j] <- x[j]
    }
  }
  ret
}

recode_integer_scale <- function(x, lowest) {
  n <- length(lowest)
  labels <- c(paste0("<=", lowest[[1L]] - 1L),
              sprintf("%d-%d", lowest[-n], lowest[-1L] - 1L),
              paste0(lowest[[n]], "+"))
  at <- c(-Inf, lowest, Inf)
  as.character(cut(x, at, right=FALSE, labels=labels))
}

recode_character_fuzzy <- function(x, valid, unmatched_action="keep", ...) {
  if (length(valid) == 1L && file.exists(valid)) {
    valid <- readLines(valid)
  }
  unmatched_action <- match.arg(unmatched_action, c("missing", "error", "keep"))
  f <- function(x) {
    i <- agrep(x, valid, ...)
    if (length(i) == 0L) {
      NA_integer_
    } else if (length(i) > 1L) {
      stop("multiple matches -- fixme")
    } else {
      i
    }
  }
  i <- vapply(unique(x), f, integer(1), USE.NAMES=FALSE)
  i <- i[match(x, unique(x))]
  j <- is.na(i) & !is.na(x)
  ret <- setNames(valid[i], names(x))
  ## NOTE: This duplicated from recode_character
  if (any(j)) {
    if (unmatched_action == "error") {
      stop("Unknown entry: ",
           paste(unique(x[j]), collapse=", "))
    } else if (unmatched_action == "keep") {
      ret[j] <- x[j]
    }
  }
  ret
}

recode_conditional <- function(x, group, table, unmatched_action="error") {
  i <- match(group, names(table))
  ## TODO: check for NA in here depending on what the unmatched action is.

  ## TODO: This conversion needs work; deal with expressions, parsed
  ## expressions, vs text etc.

  ## TODO: Do we assume these functions are vectorised?
  exprs <- lapply(table, function(x) eval(parse(text=x)))
  tmp_i <- split(seq_along(i), i)
  tmp_x <- split(x, i)

  ## TODO: type inference here is tricky.
  ret <- x
  for (j in seq_along(tmp_i)) {
    k <- tmp_i[[j]]
    ret[k] <- exprs[[j]](x[k])
  }

  ret
}

capitalise_first <- function(x, by_word=TRUE) {
  if (by_word) {
    ## This is not ideal because it will collapse spaces.  Doing
    ## this with a regular expression, e.g. on a wordbreak followed
    ## by lowercase letter would be better.
    xx <- strsplit(x, "\\s+")
    vapply(xx, function(x) paste(capitalise_first(x, FALSE), collapse=" "),
           character(1))
  } else {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  }
}

normalise_whitespace <- function(x) {
  gsub("\\s+", " ", trimws(x))
}
