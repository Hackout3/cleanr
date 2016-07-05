## TODO: export and document all of these.

## TODO: this would be nice if table could also be a filename.  If it
## is, then make it be a yml file that contains just "Bad: Good"
## matches, in the style of a normal yaml hash.

##' @importFrom stats na.omit setNames
recode_character <- function(x, table, unmatched_action="error") {
  if (possible_filename(table) && file.exists(table)) {
    table <- yaml_read(table)
  }
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
  x[x == ""] <- NA_character_
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

recode_character_fuzzy <- function(x, table_filename,
                                   unmatched_action="keep", ...) {
  if (file.exists(table_filename)) {
    table <- yaml_read(table_filename)
    valid <- na.omit(unique(vapply(table, function(x) as.character(x[[1L]]),
                                   character(1))))
    x <- recode_character(x, table, unmatched_action="keep")
  } else {
    ## TODO: Consider trying to detect the most common cases.
    valid <- character(0)
  }

  ## Empty strings cause problems.  We could recode these as "" on
  ## exit but that does not seem useful.
  x[x == ""] <- NA_character_

  ## TODO: For multimatches here, do not trigger when there is an
  ## exact match.
  xx <- unique(setdiff(na.omit(x), valid))
  ## TODO: move to proper string distances because the agrep misses
  ## some matches when the first argument is longer than the second.
  i <- lapply(xx, agrep, valid)
  n <- lengths(i)

  ## For now, give a message about match failures, later on we'll do
  ## something sensible with that in a reporting framework.
  unmatched <- n == 0L
  multimatch <- n > 1L
  matched <- n == 1L

  str <- character()

  if (any(unmatched)) {
    unmatched_n <- table(x[x %in% xx[unmatched]])
    j <- order(-unmatched_n, names(unmatched_n))
    nn <- unname(unmatched_n[j])
    str_unmatched <- sprintf("%s: !missing # (%d time%s)",
                             sanitise_yaml_key(names(unmatched_n[j])),
                             nn, ifelse(nn > 1, "s", ""))
    str <- c(str, paste0(c("# Cases with no match:", str_unmatched),
                         collapse="\n"))
  }

  if (any(multimatch)) {
    j <- order(xx[multimatch])
    multimatch_pos <-
      vapply(i[multimatch], function(k) paste(valid[k], collapse=", "),
             character(1))
    str_multimatch <-
      sprintf("%s: !missing # %s", sanitise_yaml_key(xx[multimatch][j]),
              multimatch_pos[j])
    str <- c(str, paste0(c("# Cases with more than one match:", str_multimatch),
                         collapse="\n"))
  }

  if (any(matched)) {
    f <- function(x) {
      tmp[[x]][order(drop(utils::adist(tmp[[x]], x)), decreasing=TRUE)]
    }
    tmp <- setNames(split(xx[matched], unlist(i[matched])),
                    valid[sort(unique(unlist(i[matched])))])
    tmp <- setNames(lapply(names(tmp), f), names(tmp))
    matches <- tmp[lengths(tmp) > 0]
    str_match <- vapply(names(matches), function(x)
      paste(sprintf("%s: %s", sanitise_yaml_key(matches[[x]]), x),
            collapse="\n"),
      character(1))
    str_match <- paste0(c(
      "# New possible matches (in decreasing badness within groups):",
      paste(str_match, collapse="\n\n")),
      collapse="\n")
    str <- c(str, str_match)
  }

  if (length(str) > 0L) {
    table_filename_new <- sub(".yml$", "_new.yml", table_filename)
    writeLines(paste(str, collapse="\n\n"), table_filename_new)
    msg <- c(sprintf("New entries have been added to %s\n", table_filename_new),
             sprintf("Please edit %s and add appropriate new entries and rerun",
                     table_filename))
    stop(msg, call.=FALSE)
  }

  x
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
    stringr::str_to_title(x)
  } else {
    i <- nzchar(x) & !is.na(x)
    x[i] <- paste0(toupper(substr(x[i], 1, 1)),
                   tolower(substr(x[i], 2, nchar(x))))
    x
  }
}

normalise_whitespace <- function(x) {
  gsub("\\s+", " ", trimws(x))
}

possible_filename <- function(x) {
  is.character(x) && length(x) == 1 && is.null(names(x))
}
