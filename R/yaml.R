## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list(
    "bool#yes" = function(x) {
      if (identical(toupper(x), "TRUE")) TRUE else x},
    "bool#no" = function(x) {
      if (identical(toupper(x), "FALSE")) FALSE else x},
    "missing" = function(x) { # NOTE: not yet typed...
      NA})
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call.=FALSE)
  }
  tryCatch(yaml_load(read_file(filename)),
           error=catch_yaml)
}

read_file <- function(filename, ...) {
  if (!file.exists(filename)) {
    stop(sprintf("File %s does not exist", filename))
  }
  paste(readLines(filename), collapse="\n")
}
