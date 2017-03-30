##' Bulk rename a number of columns.  Note that this overlaps
##' significantly with \code{dplyr}'s function of the same name, with
##' the chief differences being handling of yaml metadata and slightly
##' odd handling of polarity.  We may just depend on dplyr later.
##'
##' @title Rename columns
##' @param data A data.frame to modify
##' @param table A table of \code{from = to} or \code{to = from} mappings
##'
##' @param reverse_direction Logical (or NULL) to indicate the
##'   direction of the mapping.  If \code{TRUE}, then \code{table} is
##'   treated as a set of \code{to = from} mappings, while if
##'   \code{FALSE} it is treated as \code{from = to} mappings.  If
##'   \code{NULL} the direction is determined automatically.
##'
##' @param allow_missing Allow columns to be missing in \code{data}
##'   (but present \code{table}).  This is not compatible with
##'   \code{reverse_direction = NULL}.
##'
##' @export
rename <- function(data, table, reverse_direction = NULL,
                   allow_missing = FALSE) {
  if (possible_filename(table) && file.exists(table)) {
    table <- yaml_read(table)
  }
  if (is.list(table)) {
    ## TODO: validation.
    table <- unlist(table)
  }

  if (length(table) == 0L) {
    return(data)
  }

  nms <- names(data)
  if (is.null(reverse_direction)) {
    if (all(table %in% nms) && !any(names(table) %in% nms)) {
      reverse_direction <- TRUE
    } else if (all(names(table) %in% nms) && !any(table %in% nms)) {
      reverse_direction <- FALSE
    } else {
      stop("Could not guess direction")
    }
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
         paste(to[dup_to], collapse = ", "))
  }

  i <- match(from, nms)
  j <- !is.na(i)

  if (any(!j) && !allow_missing) {
    stop("Source columns not found: ", paste(from[!j], collapse = ", "))
  }

  ## This is awful indexing.  It is tested though but it's really
  ## unpleasant.  There is probably a nicer way of doing this.
  if (any(dup_from)) {
    data[to[dup_from]] <- data[i[dup_from][j[dup_from]]]
  }
  names(data)[i[!dup_from][j[!dup_from]]] <- to[!dup_from][j[!dup_from]]
  data
}
