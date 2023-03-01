#' Combine data frames by merging
#'
#' This is an extension of `merge` working for a list of data frames.
#'
#' @param data_frames [list] of [data.frame]s
#' @param id_vars [character] the variable(s) to merge the data frames by.
#'                            each of them must exist in all data frames.
#'
#' @return [data.frame] combination of data frames
util_merge_data_frame_list <- function(data_frames, id_vars) {
  util_expect_scalar(id_vars, allow_more_than_one = TRUE)
  util_stop_if_not(is.list(data_frames))
  data_frames <- data_frames[vapply(data_frames, is.data.frame,
                                    FUN.VALUE = logical(1))]
  invisible(lapply(names(data_frames),
                   function(nm) {
                     assign(nm,  # ensure a speaking error message
                            data_frames[[nm]])
                     eval(call( # ensure a speaking error message
                       "util_expect_data_frame",
                       as.symbol(nm),
                       id_vars
                     ))
                   })
            )
  all_col_names <- unlist(lapply(data_frames, colnames), recursive = TRUE)
  ambiguous_column_names <- setdiff(
    unique(all_col_names[duplicated(all_col_names)]), id_vars)
  data_frames <- lapply(setNames(nm = names(data_frames)), function(nm) {
    dfr <- data_frames[[nm]]
    acn <- intersect(ambiguous_column_names, colnames(dfr))
    dfr[, paste0(acn, ".", nm)] <-
      dfr[, acn, drop = FALSE]
    dfr[, acn] <- NULL
    dfr
  })
  res <- Reduce(function(x, y) {
    rownames(x) <- rownames(y) <- NULL
    in_both <- setdiff(intersect(colnames(x), colnames(y)), id_vars)
    res <- merge(x,
                 y,
                 by = id_vars,
                 all = TRUE)
    res
  }, data_frames)
  for (acn in ambiguous_column_names) {
    all_cns <- intersect(colnames(res), paste0(acn, ".", names(data_frames)))

    all_equal <- function(cols) {
      if (length(cols) < 2) {
        TRUE
      } else {
        all(res[, cols[[1]]] == res[, cols[[2]]], na.rm = TRUE) &&
          Recall(tail(cols, -1))
      }
    }
    if (all_equal(all_cns)) {
      res[[acn]] <- unlist(
        lapply(apply(res[, all_cns], 1, unique), setdiff, NA),
        recursive = FALSE)
      res[, all_cns] <- NULL
    }
  }
  res
}
