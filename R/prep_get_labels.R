#' Fetch a label for a variable based on its purpose
#'
#' @param resp_vars [variable list] the variable names to fetch for
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param max_len [integer] the maximum label length to return, if not possible
#'                          w/o causing ambiguous labels, the labels may still
#'                          be longer
#' @param label_class [enum] SHORT | LONG. which sort of label
#'                           according to the metadata model should be returned
#' @param label_lang [character] optional language suffix, if available in
#'                               the metadata. Can be controlled by the option
#'                               `dataquieR.lang`.
#' @param resp_vars_are_var_names_only [logical] If `TRUE`, do not use
#'                                               other labels than [VAR_NAMES]
#'                                               for finding `resp_vars` in
#'                                               `meta_data`
#' @param resp_vars_match_label_col_only [logical] If `TRUE`, do not use
#'                                               other labels than those,
#'                                               referred by `label_col`
#'                                               for finding `resp_vars` in
#'                                               `meta_data`
#'
#' @return [character] suitable labels for each `resp_vars`, names of this
#' vector are [VAR_NAMES]
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param force_label_col [enum] auto | FALSE | TRUE. if `TRUE`, always use
#'                               labels according `label_col`, `FALSE` means
#'                               use labels matching best the function's
#'                               requirements, `auto` means `FALSE`, if in a
#'                               `dq_report()` and `TRUE`, otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' prep_get_labels("SEX_0", label_class = "SHORT", max_len = 2)
#' }
prep_get_labels <- function(
    resp_vars,
    item_level = "item_level",
    label_col,
    max_len = MAX_LABEL_LEN,
    label_class = c("SHORT", "LONG"),
    label_lang = getOption("dataquieR.lang", ""),
    resp_vars_are_var_names_only = FALSE,
    resp_vars_match_label_col_only = FALSE,
    meta_data = item_level,
    meta_data_v2,
    force_label_col = getOption("dataquieR.force_label_col",
                                dataquieR.force_label_col_default)) {

  if (!missing(item_level) && !missing(meta_data) &&
      !identical(item_level, meta_data)) {
    util_error(c("You cannot provide both, %s as well as %s",
                 "these arguments are synonyms and must be",
                 "used mutually exclusively"),
               sQuote("item_level"),
               sQuote("meta_data"))
    # see prep_prepare_dataframes
  }

  util_maybe_load_meta_data_v2()

  util_expect_scalar(force_label_col,
                     check_type = is.character,
                     error_message =
                       "force_label_col must be 1 character value")

  util_match_arg(force_label_col, c(
    "auto", "FALSE", "TRUE"
  ))

  util_expect_scalar(max_len,
                     check_type = util_is_numeric_in(min = 1,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  label_class <- util_match_arg(label_class)
  util_expect_scalar(label_class)
  if (!missing(label_col)) {
    util_expect_scalar(label_col,
                       check_type = is.character,
                       error_message =
                         sprintf("%s needs to be a character string.",
                                 sQuote("label_col")))
    no_lc <- FALSE
  } else {
    no_lc <- TRUE
    label_col <- VAR_NAMES
  }
  util_expect_scalar(label_lang,
                     check_type = is.character,
                     error_message =
                       sprintf("%s needs to be a character string.",
                               sQuote("label_lang")))
  util_expect_scalar(resp_vars_are_var_names_only,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be a logical value.",
                               sQuote("resp_vars_are_var_names_only")))
  util_expect_scalar(resp_vars_match_label_col_only,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be a logical value.",
                               sQuote("resp_vars_match_label_col_only")))
  if (resp_vars_match_label_col_only &&
      resp_vars_are_var_names_only) {
    util_error("Call with either %s or %s set TRUE, not both",
               sQuote("resp_vars_match_label_col_only"),
               sQuote("resp_vars_match_label_col_only"))
  }
  util_expect_data_frame(
    meta_data,
    col_names = VAR_NAMES)

  if (missing(resp_vars)) {
    resp_vars <- meta_data[[VAR_NAMES]]
    resp_vars_are_var_names_only <- TRUE
    resp_vars_match_label_col_only <- FALSE
  }

  util_expect_scalar(resp_vars,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     allow_na = TRUE)

  if (!no_lc && !label_col %in% colnames(meta_data)) {
    util_error("%s = %s, if specified, needs to exist in %s.",
               sQuote("label_col"),
               dQuote(label_col),
               sQuote("meta_data"))
  }

  if (trimws(toupper(force_label_col)) == "AUTO") {
    force_label_col <- !no_lc && !.called_in_pipeline
  } else {
    force_label_col <- trimws(toupper(force_label_col)) == "TRUE"
  }

  if (!resp_vars_are_var_names_only &&
      !resp_vars_match_label_col_only) {
    vns <- suppressMessages(util_find_var_by_meta(
      label_col = label_col,
      resp_vars = resp_vars,
      ifnotfound = resp_vars,
      meta_data = meta_data,
      target = VAR_NAMES
    ))
  } else {
    if (resp_vars_match_label_col_only) {
      vns <- suppressMessages(prep_map_labels(
        x = resp_vars,
        meta_data = meta_data,
        from = label_col,
        to = VAR_NAMES,
        ifnotfound = resp_vars,
        warn_ambiguous = FALSE
      ))

    } else {
      vns <- suppressMessages(prep_map_labels(
        x = resp_vars,
        meta_data = meta_data,
        to = VAR_NAMES,
        ifnotfound = resp_vars,
        warn_ambiguous = FALSE
      ))
    }
  }

  if (force_label_col) {
    to <- label_col
  } else {
    if (label_class == "SHORT") {
      to <- c(LABEL, LONG_LABEL, VAR_NAMES)
    } else if (label_class == "LONG") {
      to <- c(LONG_LABEL, LABEL, VAR_NAMES)
    } else {
      # internal error
      util_error("Please excuse and inform us about Internal error 439285")
    }
  }

  to <- c(paste(to, label_lang, sep = "_"), to)  #see what happens with label_lang

  to <- c(to, gsub("_.+?$", "", setdiff(to, c(VAR_NAMES, LONG_LABEL))))

  to <- head(
    intersect(
      to,
      colnames(meta_data)),
    1)

  if (length(to) == 0) {
    return(setNames(nm = resp_vars))
  }

  uel <- util_ensure_label(meta_data[meta_data[[VAR_NAMES]] %in% vns, ,  FALSE],
                    label_col = to,
                    max_label_len = max_len)

  res <- prep_map_labels( # do not attach "to" as the used label col if the follwoing fails or vn was already not a.
    x = vns,
    meta_data = uel$meta_data,
    to = uel$label_col,
    ifnotfound = vns,
    warn_ambiguous = FALSE
  )

  res[util_empty(res)] <- resp_vars[util_empty(res)]

  util_attach_attr(res, # see above
                   label_col = to)

}
