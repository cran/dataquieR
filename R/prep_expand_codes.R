#' Expand code labels across variables
#'
#' Code labels are copied from other
#' variables, if the code is the same and the
#' label is set only for some variables
#'
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param suppressWarnings [logical] show warnings, if labels are expanded
#' @param mix_jumps_and_missings [logical] ignore the class of the codes for
#'                               label expansion, i.e., use missing code labels
#'                               as jump code labels, if the values are the
#'                               same.
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return [data.frame] an updated metadata data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' meta_data <- prep_get_data_frame("meta_data")
#' meta_data$JUMP_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
#' md <- prep_expand_codes(meta_data)
#' md$JUMP_LIST
#' md$MISSING_LIST
#' md <- prep_expand_codes(meta_data, mix_jumps_and_missings = TRUE)
#' md$JUMP_LIST
#' md$MISSING_LIST
#' meta_data <- prep_get_data_frame("meta_data")
#' meta_data$MISSING_LIST[meta_data$VAR_NAMES == "v00003"] <- "99980 = NOOP"
#' md <- prep_expand_codes(meta_data)
#' md$JUMP_LIST
#' md$MISSING_LIST
#' }
prep_expand_codes <- function(item_level = "item_level",
                              suppressWarnings = FALSE,
                              mix_jumps_and_missings = FALSE,
                              meta_data_v2,
                              meta_data = item_level) {
  util_maybe_load_meta_data_v2()
  util_expect_scalar(suppressWarnings, check_type = is.logical)
  util_expect_scalar(mix_jumps_and_missings, check_type = is.logical)
  util_expect_data_frame(meta_data, MISSING_LIST)

  Xcause_label_df <- prep_extract_cause_label_df(meta_data = meta_data,
                                                label_col = VAR_NAMES)

  cause_label_df <- Xcause_label_df$cause_label_df


  cause_label_df$AUTO <- cause_label_df$CODE_LABEL ==
    paste(cause_label_df$CODE_CLASS, cause_label_df$CODE_VALUE)


  if (mix_jumps_and_missings) {
    s_cause_label_df <- split(cause_label_df,
                              list(cause_label_df$CODE_VALUE))
  } else {
    s_cause_label_df <- split(cause_label_df,
                              list(cause_label_df$CODE_VALUE,
                                   cause_label_df$CODE_CLASS))
  }

  expand <- function(cldf) {
    have_auto <- any(cldf$AUTO)
    my_labels <- cldf[!cldf$AUTO, CODE_LABEL, TRUE]
    my_labels <- trimws(my_labels[!is.na(my_labels)])
    if (length(unique(my_labels)) == 1) {
      if (!suppressWarnings && have_auto) {
        util_message("Expand label %s for all values coded with %s",
                     dQuote(unique(my_labels)),
                     dQuote(unique(cldf$CODE_VALUE)),
                     applicability_problem = TRUE)
      }
      cldf$CODE_LABEL <- rep(unique(my_labels), nrow(cldf))
    }
    cldf
  }
  cause_label_df <-
    do.call(rbind.data.frame, lapply(s_cause_label_df, expand))

  meta_data <- prep_add_cause_label_df(Xcause_label_df$meta_data,
                                       cause_label_df,
                                       assume_consistent_codes = TRUE,
                                       replace_meta_data = FALSE)
  meta_data
}
