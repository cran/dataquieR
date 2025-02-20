#' Add data frames to the pre-loaded / cache data frame environment
#'
#' These can be referred to by their names, then, wherever `dataquieR` expects
#' a [data.frame] -- just pass a character instead. If this character is not
#' found, `dataquieR` would additionally look for files with the name and for
#' `URLs`. You can also refer to specific sheets of a workbook or specific
#' object from an `RData` by appending a pipe symbol and its name. A second
#' pipe symbol allows to extract certain columns from such sheets (but
#' they will remain data frames).
#'
#' @param ... data frames, if passed with names, these will be the names
#'            of these tables in the data frame environment. If not, then the
#'            names in the calling environment will be used.
#' @param data_frame_list a named list with data frames. Also these will be
#'                        added and names will be handled as for the `...`
#'                        argument.
#'
#' @return [data.frame] `invisible(the cache environment)`
#' @export
#' @seealso [prep_load_workbook_like_file]
#' @seealso [prep_get_data_frame]
#' @family data-frame-cache
prep_add_data_frames <- function(..., data_frame_list = list()) {
  ellipse <- list(...)
  if (is.null(names(ellipse))) {
    names(ellipse) <- rep("", length(ellipse))
  }
  unnamed <- !nzchar(names(ellipse))
  symbols <- sys.call()[-1]
  if ("data_frame_list" %in% names(symbols)) {
    symbols <- as.character(
      symbols[-which(names(symbols) == "data_frame_list")])
  } else {
    symbols <- as.character(symbols)
  }
  names(ellipse)[unnamed] <- symbols[unnamed]
  if (!is.list(data_frame_list)) {
    util_error("%s needs to be a list, if given", dQuote(data_frame_list))
  }
  symbols <- as.character(substitute(data_frame_list))[-1]
  unnamed <- !nzchar(names(data_frame_list))
  names(data_frame_list)[unnamed] <- symbols[unnamed]
  data_frame_list <- c(ellipse, data_frame_list)
  named <- nzchar(names(data_frame_list))
  if (!all(named)) {
    util_error(
      c("Some unnamed data frames could not even automatically be named",
        "Have you passed an empty string?")
    )
  }
  characters <- vapply(data_frame_list, is.character, FUN.VALUE = logical(1)) &
    (1 == vapply(data_frame_list, length, FUN.VALUE = integer(1)))
  data_frame_list[characters] <- lapply(data_frame_list[characters],
                                        prep_get_data_frame)
  nulls <- vapply(data_frame_list, is.null, FUN.VALUE = logical(1))
  data_frame_list <- data_frame_list[!nulls]
  errors <- !vapply(data_frame_list, is.data.frame, FUN.VALUE = logical(1))

  data_frame_list[errors] <- lapply(data_frame_list[errors], function(x) {
    if (is.null(dim(x))) {
      try(stop("Dimension-less object is not a data frame"), silent = TRUE)
    } else {
      try(as.data.frame(x), silent = TRUE)
    }
  })

  errors <- !vapply(data_frame_list, is.data.frame, FUN.VALUE = logical(1))

  if (any(errors)) {
    util_error(
      "Not all entries are data frames or loadable file names/URLs: %s",
      paste0(dQuote(names(data_frame_list)[errors]), collapse = ", "))
  }
  for (n in names(data_frame_list)) {
    assign(
      n,
      data_frame_list[[n]],
      envir = .dataframe_environment())
  }
  invisible(.dataframe_environment())
}
