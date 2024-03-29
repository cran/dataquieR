#' Read data from files/URLs
#'
#' data_frame_name can be a file path or an URL you can append a pipe and a
#' sheet name for Excel files or object name e.g. for `RData` files. Numbers
#' may also work. All file formats supported by your `rio` installation will
#' work.
#'
#' The data frames will be cached automatically, you can define an alternative
#' environment for this using the argument `.data_frame_list`, and you can purge
#' the cache using [prep_purge_data_frame_cache].
#'
#' Use [prep_add_data_frames] to manually add data frames to the
#' cache, e.g., if you have loaded them from more complex sources, before.
#'
#' @param data_frame_name [character] name of the data frame to read, see
#'   details
#' @param .data_frame_list [environment] cache for loaded data frames
#' @param keep_types [logical] keep types as possibly defined in a file, if the
#'                             data frame is loaded from one. set `TRUE` for
#'                             study data.
#'
#' @return [data.frame] a data frame
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_load_workbook_like_file]
#' @family data-frame-cache
#'
#' @examples
#' \dontrun{
#' bl <- as.factor(prep_get_data_frame(
#'   paste0("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus",
#'     "/Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
#'     "publicationFile|COVID_Todesfälle_BL|Bundesland"))[[1]])
#'
#' n <- as.numeric(prep_get_data_frame(paste0(
#'   "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/",
#'   "Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
#'   "publicationFile|COVID_Todesfälle_BL|Anzahl verstorbene",
#'   " COVID-19 Fälle"))[[1]])
#' plot(bl, n)
#' # Working names would be to date (2022-10-21), e.g.:
#' #
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \\
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/  \\
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|2
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \\
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|name
#' # study_data
#' # ship
#' # meta_data
#' # ship_meta
#' #
#' prep_get_data_frame("meta_data | meta_data")
#' prep_get_data_frame(file.path(system.file(package = "dataquieR"),
#'   "extdata", "meta_data.RData"))
#' prep_get_data_frame(file.path(system.file(package = "dataquieR"),
#'   "extdata", "meta_data.RData|meta_data"))
#' }
prep_get_data_frame <- function(data_frame_name,
                                .data_frame_list = .dataframe_environment,
                                keep_types = FALSE) {
  # IDEA: also allow to address rows or cells from dataframes using the syntax in data_frame_name

  if (!missing(keep_types)) {
    util_expect_scalar(keep_types,
                       check_type = is.logical,
                       error_message =
                         sprintf("%s needs to be to be a logical value.",
                                 sQuote("keep_types")))
  }

  data_frame_name <- gsub("\\s*\\|\\s*", "|", data_frame_name)

  util_expect_scalar(data_frame_name, check_type = is.character)
  if (!is.environment(.data_frame_list)) {
    util_error("%s must be an environment, if specified",
               sQuote(".data_frame_list"))
  }

  if (exists(data_frame_name, envir = .data_frame_list, mode = "list")) {
    r <- get(data_frame_name, envir = .data_frame_list, mode = "list")
    if (is.data.frame(r)) {
      return(r)
    }
  }

  # util_ensure_suggested(c("rio"), "Load data from files")

  fn <- data_frame_name
  which <- NULL
  col <- NULL

  if (any(grepl(SPLIT_CHAR, data_frame_name, fixed = TRUE))) {
    splitted <- trimws(strsplit(data_frame_name, SPLIT_CHAR, fixed = TRUE)[[1]])

    if (length(splitted) > 2) { # 1-column data frame requested
      col <- tail(splitted, 1)
      splitted <- head(splitted, -1)
    }

    which <- tail(splitted, 1)
    suppressWarnings(
      if (is.finite(as.integer(which)) && as.integer(which) < 500) {
        which <- as.integer(which)
      }
    )
    fn <- paste0(head(splitted, -1), collapse = SPLIT_CHAR)
  }

  if (is.null(which)) {
    r <- util_rio_import(fn, keep_types = keep_types)
  } else {
    r <- util_rio_import(fn, keep_types = keep_types,
                         which = which)
  }

  if (inherits(r, "try-error") || !is.data.frame(r)) {
    fn0 <- system.file("extdata", paste0(fn, ".RDS"), package = "dataquieR")
    if (file.exists(fn0)) {
      fn <- fn0
    } else {
      fn0 <- system.file("extdata", paste0(fn, ".RData"), package = "dataquieR")
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

    if (is.null(which)) {
      r <- util_rio_import(fn, keep_types = keep_types)
    } else {
      r <- util_rio_import(fn, keep_types = keep_types,
                           which = which)
    }

    if (inherits(r, "try-error")) {
      if (conditionMessage(attr(r, "condition")) ==
          "No such file" && length(prep_list_dataframes()) == 0) {
        hint <- sprintf("\nDid you forget to call %s, %s or %s?",
                        sQuote("prep_load_workbook_like_file"),
                        sQuote("prep_add_data_frames"),
                        sQuote("prep_load_folder_with_metadata")
        )
      } else {
        hint <- ""
      }
      util_error("Cannot read file %s using %s: %s%s",
                 dQuote(fn),
                 sQuote("rio"),
                 conditionMessage(attr(r, "condition")),
                 hint)
    }

    if (!is.data.frame(r)) {
      util_error("File %s did not contain a table (data frame) according to %s",
                 dQuote(fn),
                 sQuote("rio"))
    }
  }

  if (!is.null(col)) {
    if (!(col %in% colnames(r))) {
      util_error("%s does not contain a column named %s on/in %s",
                 dQuote(fn),
                 dQuote(col),
                 dQuote(which))
    }
    r <- r[, col, drop = FALSE]
  }



  assign(data_frame_name, r, envir = .data_frame_list)

  return(r)
}

.dataframe_environment <- new.env(parent = emptyenv())
