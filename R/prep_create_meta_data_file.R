#' Instantiate a new metadata file
#'
#' @param file_name [character] file path to write to
#' @param study_data [data.frame] optional, study data to guess metadata from
#' @param open [logical] open the file after creation
#' @param overwrite [logical] overwrite `file`, if exists
#'
#' @return `invisible(NULL)`
#' @export
prep_create_meta_data_file <- function(file_name,
                                       study_data,
                                       open = TRUE,
                                       overwrite = FALSE) {
  util_expect_scalar(file_name,
                     check_type = is.character,
                     error_message =
                       sprintf("%s needs to be a character string.",
                               sQuote("file_name")))
  util_expect_scalar(open,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be logical",
                               sQuote("open")))

  util_expect_scalar(overwrite,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be logical",
                               sQuote("overwrite")))

  if (!missing(study_data)) {
    util_expect_data_frame(study_data, keep_types = TRUE)
  }

  if (!overwrite && file.exists(file_name)) {
    cll <- sys.call()
    if (identical(as.character(cll[[1]]), "prep_create_meta_data_file")) {
      # not called as dataquieR::prep_create_meta_data_file, but
      # as prep_create_meta_data_file()
      prefix <- paste0(packageName(), "::")
    } else {
      prefix <- ""
    }
    cmd <- paste0(deparse(nlines = -1,
                   rlang::call_modify(cll,
                                      overwrite = TRUE)),
                  collapse = "\n")
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      util_error(
        cli::cli_text(
          sprintf(
"%s already exists, will not overwrite. Call \n{.run [%s](%s%s)}\n to overwrite.",
    dQuote(file_name), cmd, prefix, cmd))
      )
    } else {
      util_error(
        "%s already exists, will not overwrite. Call \n%s\n to overwrite.",
        dQuote(file_name),
        cmd
      )
    }
  }
  mdl <- util_rio_import_list(system.file("extdata",
                               "meta_data_v2.xlsx",
                               package = packageName()))

  if (!missing(study_data)) {
    old_cache <- prep_list_dataframes()
    mdl[["item_level"]] <-
      prep_study2meta(study_data = study_data)

    mdl[["item_level"]] <-
      prep_meta_data_v1_to_item_level_meta_data(
        meta_data = mdl[["item_level"]],
        verbose = FALSE)

    mdl[["item_level"]][[MISSING_LIST]] <- NULL
    mdl[["item_level"]][[JUMP_LIST]] <- NULL

    mlts <- unique(mdl[["item_level"]][[MISSING_LIST_TABLE]])

    for (mlt in mlts) {
      if (!is.na(mlt)) try(mdl[[mlt]] <- prep_get_data_frame(mlt),
                           silent = TRUE)
    }
    new_cache <- prep_list_dataframes()
    rm(list = setdiff(new_cache, old_cache), envir = .dataframe_environment)
  }

  e <- try(rio::export(mdl,
                            file = file_name),
           silent = TRUE)
  if (inherits(e, "try-error")) {
    util_error("Could not write %s: %s",
               dQuote(file_name),
               conditionMessage(attr(e, "condition")))
  }
  if (open) {
    browseURL(file_name)
  }
  invisible(NULL)
}
