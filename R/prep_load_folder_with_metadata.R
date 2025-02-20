#' Pre-load a folder with named (usually more than) one table(s)
#'
#' These can thereafter be referred to by their names only. Such files are,
#' e.g., spreadsheet-workbooks or `RData`-files.
#'
#' Note, that this function in contrast to [prep_get_data_frame] does neither
#' support selecting specific sheets/columns from a file.
#'
#' @param folder the folder name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param ... arguments passed to []
#'
#' @return `invisible(the cache environment)`
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_get_data_frame]
#' @family data-frame-cache
prep_load_folder_with_metadata <- function(folder,
                                         keep_types = FALSE,
                                         ...) {

  util_expect_scalar(folder, check_type = is.character)
  util_stop_if_not(
    "full.names not supported by prep_load_folder_with_metadata" =
                     (!"full.names" %in% rlang::call_args_names(
                       rlang::current_call())))
# IDEA: If folder is actually a zip file with zip extensions, uncompress first
# IDEA: If an html file is loaded, extract tables from it
# IDEA: Parse data:-URLs
# IDEA: Support iframes
  if (startsWith(folder, "https://") ||
      startsWith(folder, "http://")) {
    fp <- tempfile()
    util_stop_if_not(!file.exists(fp))
    dir.create(fp)
    on.exit(try(unlink(fp, force = TRUE, recursive = TRUE, expand = FALSE),
                silent = TRUE))

    file_new <- file.path(fp, "index.html")
    try(utils::download.file(folder, destfile = file_new,
                             quiet = TRUE, mode = "wb"), silent = TRUE)
    if (file.exists(file_new)) {
      fl <- try(rvest::read_html(file_new), silent = TRUE)
      if (inherits(fl, "try-error")) {
        util_error("Could not read index from %s (%s): %s",
                   dQuote(folder),
                   dQuote(file_new),
                   conditionMessage(attr(fl, "condition")))
      }
      links <- rvest::html_nodes(fl, "a")
      all_refs <- rvest::html_attr(links, 'href')
      all_refs[!startsWith(tolower(all_refs), "http://") &
                 !startsWith(tolower(all_refs), "https://")] <-
        paste0(folder,
               "/", all_refs[!startsWith(tolower(all_refs), "http://") &
                           !startsWith(tolower(all_refs), "https://")])
      all_refs <- trimws(all_refs)
      lapply(
        all_refs,
        function(ref) {
          #ref_dec <- utils::URLdecode(ref)
          rf <- gsub("^.*\\/", "", ref, perl = TRUE)
          rf <- gsub("\\?.*$", "", rf)
          rf <- gsub("#.*$", "", rf)
          #rf_path <- gsub(print(rf), "", ref)
          ext <- ""
          ext <- try(util_fetch_ext(ref), silent = TRUE)
          # do not ignore content-disposition headers sent by the server (if they propose a file name)
          if (length(ext) != 1 ||
              !is.character(ext)) {
            msg <- "unknown reason"
            if (inherits(ext, "try-error")) {
              msg <- conditionMessage(attr(ext, "condition"))
            } else if (inherits(ext, "condition")) {
              msg <- conditionMessage(ext)
            }
            util_warning("Could not determine the file type of %s: %s",
                         dQuote(ref),
                         sQuote(msg))
            ext <- ""
          } else {
            if (!is.null(attr(ext, "file-name"))) {
              if (length(attr(ext, "file-name")) == 1 &&
                  !is.na(attr(ext, "file-name")))
                rf <- attr(ext, "file-name")
            }
            ext <- paste0(".", ext)
          }

          if (!endsWith(rf, ext)) {
            rf <- paste0(rf, ext)
          }

          try(utils::download.file(ref,
                                   destfile = file.path(fp, rf),
                                   quiet = TRUE, mode = "wb"),
              silent = TRUE)
        }
      )
      unlink(file_new, force = TRUE)
      folder <- fp
    }
  }

  util_stop_if_not(`Folder not found` = dir.exists(folder))
  util_stop_if_not(`Access denied` = file.access(folder) == 0)

  fls <- list.files(folder,
                    full.names = TRUE,
                    ...)

  lapply(fls, function(fn) {
    if (inherits(suppressWarnings(try(prep_load_workbook_like_file(fn,
                                                                   keep_types = keep_types),
                                      silent = TRUE)),
        "try-error")) {
      if (inherits(suppressWarnings(try(prep_get_data_frame(fn,
                                                            keep_types = keep_types),
                                        silent = TRUE)),
          "try-error")) {
        util_warning("Could not load %s, ignoring...", dQuote(fn))
      }
    }
  })

  invisible(.dataframe_environment())
}
