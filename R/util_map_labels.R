#' @inherit prep_map_labels
#'
#' @family mapping
#' @concept metadata_management
#' @keywords internal
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

  nm <- meta_data[[from]]
  isempty <- util_empty(nm)
  nempty <- sum(isempty)

  i <- 0
  replnm <- paste0("..", i + seq_len(nempty))
  while (length(intersect(replnm, union(nm, x))) > 0) {
    i <- i + 1
    replnm <- paste0("..", i + seq_len(nempty))
  }

  nm[isempty] <- replnm

  x[!nzchar(as.character(x))] <- NA_character_

  unlist(mget(as.character(x),
              as.environment(as.list(
                setNames(as.character(meta_data[[to]]),
                         nm = nm)
              )),
              ifnotfound = ifnotfound))
}
