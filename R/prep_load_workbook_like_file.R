#' Pre-load a file with named (usually more than) one table(s)
#'
#' These can thereafter be referred to by their names only. Such files are,
#' e.g., spreadsheet-workbooks or `RData`-files.
#'
#' Note, that this function in contrast to [prep_get_data_frame] does neither
#' support selecting specific sheets/columns from a file.
#'
#' @param file the file name to load.
#'
#' @return [data.frame] `invisible(the cache environment)`
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_get_data_frame]
#' @family data-frame-cache
#' @examples
#' \dontrun{
#' file_name <-
#'   system.file("extdata", "meta_data_extended.xlsx", package = "dataquieR")
#' prep_load_workbook_like_file(file_name)
#' prep_get_data_frame(
#'   "dataframe_level") # dataframe_level is a sheet in the file
#' }
prep_load_workbook_like_file <- function(file) {
  # util_ensure_suggested(c("rio"), "Load data from files")

  util_expect_scalar(file, check_type = is.character)

  if (startsWith(file, "https://") ||
      startsWith(file, "http://")) {
    fp <- tempfile()
    util_stop_if_not(!file.exists(fp))
    dir.create(fp)
    on.exit(try(unlink(fp, force = TRUE, recursive = TRUE, expand = FALSE), silent = TRUE))
    file_new <- file.path(fp, gsub("^.*\\/", "", file, perl = TRUE))
    try(utils::download.file(file, destfile = file_new, quiet = TRUE), silent = TRUE)
    if (file.exists(file_new)) {
      file <- file_new
    }
  }

  fn <- file

  r <-
    suppressMessages(suppressWarnings(try(rio::import_list(fn), silent = TRUE)))

  if (inherits(r, "try-error") || !is.list(r) ||
      any(vapply(r, is.data.frame, FUN.VALUE = logical(1)))) {
    fn0 <- system.file("extdata", paste0(fn, ".RDS"),
                         package = "dataquieR")
    if (file.exists(fn0)) {
      fn <- fn0
    } else {
      fn0 <- system.file("extdata", paste0(fn, ".RData"),
                           package = "dataquieR")
      if (file.exists(fn0)) {
        fn <- fn0
      } else {
        fn0 <- system.file("extdata", paste0(fn, ".xlsx"),
                             package = "dataquieR")
        if (file.exists(fn0)) {
          fn <- fn0
        }
      }
    }
    r <- suppressMessages(suppressWarnings(
      try(rio::import_list(fn), silent = TRUE)))
  }

  if (inherits(r, "try-error") || !is.list(r) ||
      any(
        !vapply(r, is.null, FUN.VALUE = logical(1))
        &
        !vapply(r, is.data.frame, FUN.VALUE = logical(1)))) {
    if (inherits(r, "try-error")) {
      error <- gsub("%", "%%", fixed = TRUE,
                          conditionMessage(attr(r, "condition")))
    } else if (file.access(fn, 4)) { # file.access has inverse result logics
      error <- "Access to denied"
    } else if (!file.exists(fn)) {
      error <- "File not found"
    } else if (dir.exists(fn)) {
      error <- "It is a folder"
    } else if (any(
      !vapply(r, is.null, FUN.VALUE = logical(1))
      &
      !vapply(r, is.data.frame, FUN.VALUE = logical(1)))) {
      error <-
        "File does not feature a workbook-like structure, i.e., a named list of data frames"
    } else {
      error <- "Unkonwn reasons."
    }
    util_error("Could not load/find file %s: %s", dQuote(file),
               error)
  }
  data_frame_list <- r
  prep_add_data_frames(data_frame_list = data_frame_list)
  data_frame_list2 <- data_frame_list
  names(data_frame_list2) <- paste0(basename(fn),
                                    SPLIT_CHAR,
                                    names(data_frame_list2))
  r <- prep_add_data_frames(data_frame_list = data_frame_list2)
  trunc_name <- basename(fn)
  if (all(grepl("\\.(RDS|RData|xlsx)$", trunc_name, perl = TRUE))) {
    trunc_name <- gsub("\\.(RDS|RData|xlsx)$", "", trunc_name, perl = TRUE)
    data_frame_list3 <- data_frame_list
    names(data_frame_list3) <- paste0(trunc_name,
                                      SPLIT_CHAR,
                                      names(data_frame_list3))
    r <- prep_add_data_frames(data_frame_list = data_frame_list3)
  }
  invisible(r)
}
