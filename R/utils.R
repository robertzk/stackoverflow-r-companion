`%||%` <- function(x, y) if (is.null(x)) y else x

#' Some file in which to store temporary information used by the package
#' (such as which questions have already been seen).
registry <- function(value) {
  registry_file <- getOption('socompanion.registry') %||% '~/.socompanion')
  if (!missing(value)) saveRDS(value, registry_file)
  else if (file.exists(registry_file)) readRDS(registry_file)
  else list()
}

