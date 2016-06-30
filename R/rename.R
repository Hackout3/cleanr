## There is _massive_ overlap here with dplyr, but we'll take a
## different approach (it's not designed for interactive work but for
## bulk processing).
rename <- function(data, table) {
  if (possible_filename(table) && file.exists(table)) {
    table <- yaml_read(table)
  }
  if (is.list(table)) {
    ## TODO: validation.
    table <- unlist(table)
  }
  dup_to <- duplicated(names(table))
  if (any(dup_to)) {
    stop("Duplicated destination columns: ",
         paste(names(table)[dup_to], collapse=", "))
  }
  dup_from <- duplicated(table)
  if (length(table) > 0L) {
    i <- match(table, names(data))
    j <- is.na(i)

    if (any(j)) {
      stop("Source columns not found: ", paste(names(data)[j], collapse=", "))
    }
    if (any(dup_from)) {
      data[names(table[dup_from])] <- data[i[dup_from]]
    }
    names(data)[i[!dup_from]] <- names(table[!dup_from])
  }
  data
}
