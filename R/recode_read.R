##' Read recoding rules from a yaml file.
##' @title Read recoding rules
##' @param filename Filename to use
##'
##' @param recipes Optional filename for recipes.  If omitted and a
##'   file `recipes.yml` is found, that will be used.
##'
##' @export
recode_read <- function(filename, recipes=NULL) {
  recipes <- recode_read_recipes(filename, recipes)
  get_recipe <- function(name) {
    if (name %in% names(recipes)) {
      ret <- recipes[[name]]
      class(ret) <- "recipe"
      ret
    } else {
      stop(sprintf("Recipe %s not found", name))
    }
  }

  dat <- yaml_read(filename, "recipe" = get_recipe)
  recode_validate(dat)
}

recode_read_recipes <- function(filename, recipes=NULL) {
  recipes_default <- file.path(dirname(filename), "recipes.yml")
  if (is.null(recipes) && file.exists(recipes_default)) {
    recipes <- recipes_default
  }
  if (is.null(recipes)) {
    NULL
  } else if (!file.exists(recipes)) {
    stop(sprintf("recipes file '%s' not found", recipes))
  } else {
    dat <- yaml_read(recipes)
    recode_validate_recipes(dat)
    dat
  }
}

recode_validate_recipes <- function(dat) {
  assert_named(dat)
  lapply(dat, recode_validate1)
}

recode_validate <- function(dat) {
  if (is.null(names(dat))) {
    dat <- unlist(dat, FALSE)
  } else {
    ## TODO: this could be relaxed in future.
    stop("Expected an ordered map")
  }
  assert_named(dat)
  lapply(dat, recode_validate1)
}

recode_validate1 <- function(x) {
  ## This is a bit nasty because we deal with a few different input
  ## types here.  They'll get a workout in the tests.
  if (is.character(x)) {
    x <- setNames(rep(list(NULL), length(x)), x)
  } else if (is.list(x)) {
    ## Need to catch something here with recipes, which can end up
    ## badly spliced.
    is_recipe <- vapply(x, inherits, logical(1), "recipe")
    if (any(is_recipe)) {
      ## There's a nasty bit of expansion needed here that is probably
      ## incorrect a lot of the time, and might be in the case of
      ## rules that are a character vector?
      ret <- vector("list", length(x) - sum(is_recipe) +
                            sum(lengths(x[is_recipe])))
      j <- 1L
      for (i in seq_along(x)) {
        if (is_recipe[[i]]) {
          k <- length(x[[i]])
          ret[seq.int(j, length.out=k)] <- unclass(x[[i]])
          j <- j + k
        } else {
          ret[j] <- x[i]
          j <- j + 1L
        }
      }
      x <- ret
    }
    if (is.null(names(x))) {
      x <- from_yaml_ordered_map(x)
      assert_named(x)
    } else if (length(x) != 1L) { # TODO: length == 0?
      if (!(length(x) == 2L && "from" %in% names(x))) {
        stop("If more than entry is given, must be an ordered map")
      }
    }
  } else {
    ## TODO: diagnostics
    stop("parse error")
  }
  ## TODO: validate arguments against functions and check functions exist.
  x
}
