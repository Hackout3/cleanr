## There is _massive_ overlap here with dplyr, but we'll take a
## different approach (it's not designed for interactive work but for
## bulk processing).
rename <- function(data, table, reverse_direction=FALSE) {
  if (possible_filename(table) && file.exists(table)) {
    table <- yaml_read(table)
  }
  if (is.list(table)) {
    ## TODO: validation.
    table <- unlist(table)
  }

  if (reverse_direction) {
    from <- unname(table)
    to <- names(table)
  } else {
    to <- unname(table)
    from <- names(table)
  }

  dup_to <- duplicated(to)
  dup_from <- duplicated(from)

  if (any(dup_to)) {
    stop("Duplicated destination columns: ",
         paste(to[dup_to], collapse=", "))
  }
  if (length(from) > 0L) {
    i <- match(from, names(data))
    j <- is.na(i)

    if (any(j)) {
      stop("Source columns not found: ", paste(from[j], collapse=", "))
    }
    if (any(dup_from)) {
      data[to[dup_from]] <- data[i[dup_from]]
    }
    names(data)[i[!dup_from]] <- to[!dup_from]
  }
  data
}
