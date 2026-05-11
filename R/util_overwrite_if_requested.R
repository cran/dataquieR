util_overwrite_if_requested <- function(dir, force_overwrite) {
  if (file.exists(dir)) {
    if (force_overwrite) {
      to_remove <-
        list.files(
          path = dir, all.files = TRUE,
          full.names = TRUE,
          recursive = TRUE,
          include.dirs = TRUE,
          no.. = TRUE
        )
      # Paranoia before deletion:
      to_remove_norm <- vapply(
        to_remove,
        normalizePath,
        FUN.VALUE = character(1),
        mustWork = FALSE
      )
      if (any(unlist(
        lapply(c(to_remove_norm), strsplit, .Platform$file.sep,
               fixed = TRUE)) == ".."))
        util_error("Refusing to unlink paths containing '..'")
      unlink(to_remove_norm,
             recursive = TRUE,
             force = TRUE,
             expand = FALSE)
      if (file.exists(dir) && !dir.exists(dir)) {
        util_error(c("%s already exists as a file, not a directory, cannot",
                     "use this as an output folder."),
                   dQuote(dir))
      }
    } else {
      util_error("%s already exists, cannot use this as an output folder.",
                 dQuote(dir))
    }
  }
  if (!dir.exists(dir))
    if (!dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
      util_error("Could not create %s", dQuote(dir))
  }
}
