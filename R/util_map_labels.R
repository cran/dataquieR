#' @inherit prep_map_labels
util_map_labels <- function(x, meta_data = NULL,
                            to = LABEL, from = VAR_NAMES, ifnotfound) {
  unlist(mget(as.character(x),
              as.environment(as.list(setNames(as.character(meta_data[[to]]),
    nm = meta_data[[from]]
  ))),
  ifnotfound = ifnotfound
  ))
}
