## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
yaml_load <- function(string, ...) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list(
    "bool#yes" = function(x) {
      if (identical(toupper(x), "TRUE")) TRUE else x},
    "bool#no" = function(x) {
      if (identical(toupper(x), "FALSE")) FALSE else x},
    "missing" = function(x) { # NOTE: not yet typed...
      NA},
    ...)
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename, ...) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call.=FALSE)
  }
  tryCatch(yaml_load(read_file(filename), ...),
           error=catch_yaml)
}

read_file <- function(filename, ...) {
  if (!file.exists(filename)) {
    stop(sprintf("File %s does not exist", filename))
  }
  paste(readLines(filename), collapse="\n")
}

from_yaml_ordered_map <- function(dat, named=TRUE) {
  if (is.null(dat) || length(dat) == 0) {
    return(structure(list(), names=character(0)))
  }
  assert_list(dat)
  ## First, check that everything is length 1:
  if (!all(lengths(dat) == 1L)) {
    stop("Expected every element to be length 1")
  }
  dat_contents <- lapply(dat, function(x) x[[1L]])
  dat_names <- lapply(dat, names)
  dat_unnamed <- vapply(dat_names, is.null, logical(1L))

  ## This is possibly controversial:
  i <- dat_unnamed & vapply(dat_contents, function(x)
    is.character(x) && length(x) == 1, logical(1))
  if (any(i)) {
    dat_names[i] <- unlist(dat_contents[i])
    dat_contents[i] <- rep(list(NULL), sum(i))
    dat_unnamed[i] <- FALSE
  }

  if (named) {
    if (any(dat_unnamed)) {
      stop("All elements must be named")
    }
  } else {
    dat_names[dat_unnamed] <- dat_contents[dat_unnamed]
  }
  dat_names <- vapply(dat_names, identity, character(1L))
  names(dat_contents) <- dat_names
  dat_contents
}
