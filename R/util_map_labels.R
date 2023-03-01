#' @inherit prep_map_labels
util_map_labels <- function(x, meta_data = "item_level",
                            to = LABEL, from = VAR_NAMES, ifnotfound,
                            warn_ambiguous = FALSE) {
  util_expect_data_frame(meta_data, c(from, to))

  if (warn_ambiguous) {
    ambiguities <- vapply(as.character(x),
                          FUN.VALUE = logical(1),
                          FUN = function(xx) {
                            length(which(meta_data[[from]] == xx)) > 1
                          })
    if (any(ambiguities)) {
      util_warning(paste("There are several entries '%s' in the metadata column",
                         "%s. Mapping to metadata column %s is ambiguous."),
                   paste0(as.character(x)[ambiguities], collapse = ", "),
                   from,
                   to,
                   applicability_problem = TRUE)
    }
  }

  unlist(mget(as.character(x),
              as.environment(as.list(
                setNames(as.character(meta_data[[to]]),
                         nm = meta_data[[from]])
              )),
              ifnotfound = ifnotfound))
}
