#' Fetch a label for a variable based on its purpose
#'
#' @param resp_vars [variable list] the variable names to fetch for
#' @param meta_data [meta_data] the metadata, item-level
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param max_len [integer] the maximum label length to return, if not possible
#'                          w/o causing ambiguous labels, the labels may still
#'                          be longer
#' @param label_class [enum] SHORT | LONG. which sort of label
#'                           according to the metadata model should be returned
#' @param label_lang [character] optional language suffix, if available in
#'                               the metadata
#' @param resp_vars_are_var_names_only [logical] If `TRUE`, do not use
#'                                               other labels than [VAR_NAMES]
#'                                               for finding `resp_vars` in
#'                                               `meta_data`
#'
#' @return [character] suitable labels for each `resp_vars`, names of this
#' vector are [VAR_NAMES]
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
    meta_data = "item_level",
    label_col,
    max_len = MAX_LABEL_LEN,
    label_class = c("SHORT", "LONG"),
    label_lang = "",
    resp_vars_are_var_names_only = FALSE) {

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
  util_expect_data_frame(
    meta_data,
    col_names = VAR_NAMES)

  if (missing(resp_vars)) {
    resp_vars <- meta_data[[VAR_NAMES]]
    resp_vars_are_var_names_only <- TRUE
  }

  util_expect_scalar(resp_vars,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     allow_na = TRUE)

  if (!no_lc && !label_col %in% colnames(meta_data)) {
    util_error("%s, if specified, needs to exist in %s.")
  }
  if (!resp_vars_are_var_names_only) {
    vns <- suppressMessages(util_find_var_by_meta(
      label_col = label_col,
      resp_vars = resp_vars,
      ifnotfound = resp_vars,
      meta_data = meta_data,
      target = VAR_NAMES
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

  if (label_class == "SHORT") {
    to <- LABEL
  } else if (label_class == "LONG") {
    to <- LONG_LABEL
  } else {
    # internal error
    util_error("Please excuse and inform us about Internal error 439285")
  }

  to <- c(paste(to, label_lang, sep = "_"), to)

  to <- c(to, gsub("_.+?$", "", setdiff(to, c(VAR_NAMES, LONG_LABEL))))

  to <- head(
    intersect(
      to,
      colnames(meta_data)),
    1)

  if (length(to) == 0) {
    return(resp_vars)
  }

  res <- prep_map_labels(
    x = vns,
    meta_data =
      util_ensure_label(NULL,
                        meta_data[meta_data[[VAR_NAMES]] %in% vns, ,  FALSE],
                        label_col = to,
                        max_label_len = max_len)$meta_data,
    to = to,
    ifnotfound = vns,
    warn_ambiguous = FALSE
  )

  res[util_empty(res)] <- resp_vars[util_empty(res)]

  util_attach_attr(res,
                   label_col = to)

}
