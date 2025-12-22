#' Convert `VALUE_LABELS` to separate tables
#'
#' @inheritParams .template_function_indicator
#' @param max_value_label_len [integer] maximum length for value labels
#'
#' @return [data.frame] metadata with `VALUE_LABEL_TABLE` instead of
#'   `VALUE_LABELS` (or none of these, if absent)
#'
#' @noRd
#' @examples
#' \dontrun{
#' prep_purge_data_frame_cache()
#' prep_load_workbook_like_file("meta_data_v2")
#' util_normalize_value_labels()
#' prep_add_data_frames(test_labs =
#'   tibble::tribble(~ CODE_VALUE, ~ CODE_LABEL, 17L, "Test", 19L, "Test",
#'     17L, "TestX"))
#' il <- prep_get_data_frame("item_level")
#' if (!VALUE_LABEL_TABLE %in% colnames(il)) {
#'   il$VALUE_LABEL_TABLE <- NA_character_
#' }
#' il$VALUE_LABEL_TABLE[[1]] <- "test_labs"
#' il$VALUE_LABELS[[1]] <- "17 = TestY"
#' prep_add_data_frames(item_level = il)
#' util_normalize_value_labels()
#' }
#'
util_normalize_value_labels <- function(meta_data = "item_level",
                                        max_value_label_len =
                                          getOption(
                                            "dataquieR.MAX_VALUE_LABEL_LEN",
                                      dataquieR.MAX_VALUE_LABEL_LEN_default)) {

  util_expect_data_frame(meta_data)

  if (!any(c(VALUE_LABELS, VALUE_LABEL_TABLE) %in% colnames(meta_data))) {
    return(meta_data)
  }

  util_expect_scalar(max_value_label_len,
                     check_type = is.numeric,
                     error_message = sprintf("%s must be a numeric value",
                                             sQuote("dataquieR.MAX_VALUE_LABEL_LEN")))

  if (VALUE_LABELS %in% colnames(meta_data)) {
    if (VALUE_LABEL_TABLE %in% colnames(meta_data) &&
        any(!util_empty(meta_data[!util_empty(meta_data[[VALUE_LABELS]]),
                                  VALUE_LABEL_TABLE]))) {
      util_warning(
        "Cannot mix %s and %s, trying to fix, but expect inconsistencies.",
        sQuote(VALUE_LABELS),
        sQuote(VALUE_LABEL_TABLE), applicability_problem = TRUE)

      # load all data frames
      invisible(lapply(
        meta_data[!util_empty(meta_data[[VALUE_LABEL_TABLE]]),
                  VALUE_LABEL_TABLE, drop = TRUE], prep_get_data_frame))
    }

    md <- meta_data
    md <- md[!duplicated(md[[VALUE_LABELS]]), , FALSE]

    def_cats <- setNames(lapply(util_parse_assignments(md[[VALUE_LABELS]],
                                       split_on_any_split_char = FALSE,
                                       split_char = c(SPLIT_CHAR, '<'),
                                       multi_variate_text = TRUE),
                                function(x) {
      if (length(x) > 0) {
        r <- data.frame(CODE_VALUE = names(x),
                   CODE_LABEL = unlist(unname(x), recursive = FALSE),
                   row.names = NULL,
                   check.rows = FALSE,
                   check.names = FALSE,
                   fix.empty.names = FALSE,
                   stringsAsFactors = FALSE)
      } else {
        r <- data.frame(CODE_VALUE = character(0),
                   CODE_LABEL = character(0),
                   row.names = NULL,
                   check.rows = FALSE,
                   check.names = FALSE,
                   fix.empty.names = FALSE,
                   stringsAsFactors = FALSE)
      }
      if (attr(x, "split_char") == '<') {
        r[[CODE_ORDER]] <- rank(r[[CODE_VALUE]], ties.method = "random")
      }
      return(r)
    }), nm = md[[VALUE_LABELS]])

    names(def_cats) <- .util_generate_value_label_table_name(names(def_cats))

    if (any(names(def_cats) %in% prep_list_dataframes())) {
      already <-
        names(def_cats)[names(def_cats) %in% prep_list_dataframes()]
      for (vlt in already) {
        def_cats[[vlt]] <-
          util_combine_value_label_tables(prep_get_data_frame(vlt),
                                          def_cats[[vlt]])
      }
    }

    prep_add_data_frames(data_frame_list = def_cats)
  }

  if (!(VALUE_LABELS %in% colnames(meta_data))) {
    meta_data[[VALUE_LABELS]] <- NA_character_
  }

  if (!(VALUE_LABEL_TABLE %in% colnames(meta_data))) {
    meta_data[[VALUE_LABEL_TABLE]] <- NA_character_
  }

  # fix records with VALUE_LABELS and VALUE_LABEL_TABLE
  both <-
    !util_empty(meta_data[[VALUE_LABELS]]) &
      !util_empty(meta_data[[VALUE_LABEL_TABLE]])

  for (i in which(both)) {
    orig_nm <- meta_data[[VALUE_LABEL_TABLE]][[i]]
    cmbd_nm <- paste0(orig_nm, "_",
                       VALUE_LABELS)

    vlnm <-
      .util_generate_value_label_table_name(meta_data[[VALUE_LABELS]][[i]])

    vldf <- def_cats[[vlnm]]

    if ((cmbd_nm %in% prep_list_dataframes())) {

      exdf <- prep_get_data_frame(cmbd_nm)

      testdf <- merge(vldf, exdf, by = intersect(c(CODE_VALUE, CODE_ORDER), # FIXME: CODE_ORDER: honor its values, not only its existence
                                                 intersect(colnames(vldf),
                                                           colnames(exdf))),
                      all = TRUE)

      if (nrow(testdf) != nrow(exdf) || nrow(testdf) != nrow(vldf)) {
        # util_warning(
        #   "Will overwrite the following data frame: %s",
        #   dQuote(cmbd_nm), applicability_problem = TRUE)
      }
    }

    meta_data[[VALUE_LABELS]][[i]] <- NA
    meta_data[[VALUE_LABEL_TABLE]][[i]] <- cmbd_nm

    prep_add_data_frames(data_frame_list = setNames(
      list(x = util_combine_value_label_tables(
        prep_get_data_frame(orig_nm),
        vldf)),
      nm = cmbd_nm
    ))

  }

  meta_data[!util_empty(meta_data[[VALUE_LABELS]]),
            VALUE_LABEL_TABLE] <-
    .util_generate_value_label_table_name(
      meta_data[!util_empty(meta_data[[VALUE_LABELS]]), VALUE_LABELS])

  meta_data[[VALUE_LABELS]] <- NULL

  is_too_long <- function(x) {
    nchar(x) > max_value_label_len
  }
  to_fix <- unique(meta_data[[VALUE_LABEL_TABLE]]) # TODO: Standardized voc;
  to_fix <- to_fix[!is.na(to_fix)]
  lapply(to_fix, function(vlt) {
    cur_df <- try(prep_get_data_frame(vlt), silent = TRUE)
    if (is.data.frame(cur_df)) {
      if (!(CODE_LABEL %in% colnames(cur_df)) &&
          CODE_VALUE %in% colnames(cur_df) &&
          any(is_too_long(cur_df[[CODE_VALUE]]))) {
        cur_df[[CODE_LABEL]] <- cur_df[[CODE_VALUE]]
      }
      if (CODE_LABEL %in% colnames(cur_df) &&
          any(is_too_long(cur_df[[CODE_LABEL]]))) {
        cur_df[is_too_long(cur_df[[CODE_LABEL]]), CODE_LABEL] <-
          util_abbreviate_unique(
            cur_df[is_too_long(cur_df[[CODE_LABEL]]), CODE_LABEL],
            max_value_label_len = max_value_label_len)
      }
      prep_add_data_frames(data_frame_list =
                             setNames(list(cur_df), nm = vlt))
      # } else if (util_is_try_error(cur_df)) { this will be shown anyways
      #   util_warning("Could not find value label table %s",
      #                applicability_problem = TRUE)
    }
  })

  meta_data

}

#' Combine two value lists
#'
#' @param vlt1 [value_label_table]
#' @param vlt2 [value_label_table]
#'
#' @return [value_label_table]
#' @noRd
#' @examples
#' \dontrun{
#' util_combine_value_label_tables(
#'   tibble::tribble(~ CODE_VALUE, ~ CODE_LABEL, 17L, "Test", 19L, "Test", 17L, "TestX"),
#'   tibble::tribble(~ CODE_VALUE, ~ CODE_LABEL, 17L, "Test", 19L, "Test", 17L, "TestX"))
#' }
util_combine_value_label_tables <- function(vlt1, vlt2) {
  vlt <- unique(util_rbind(vlt1, vlt2))
  if (any(duplicated(vlt[[CODE_VALUE]]))) {
    # util_warning(c("Duplicated %ss detected: %s. Fixing",
    #                "by merging the labels"),
    #              sQuote(CODE_VALUE),
    #              util_pretty_vector_string(
    #                unique(vlt[[CODE_VALUE]][duplicated(vlt[[CODE_VALUE]])])
    #              ),
    #              applicability_problem = TRUE)
    dups <- split(vlt, vlt[[CODE_VALUE]])
    dups <- lapply(dups, function(dup) {
      dup[[CODE_LABEL]] <-
        common_label <- paste(dup[[CODE_LABEL]],
                              collapse = sprintf(" %s ", SPLIT_CHAR))
      dup
    })
    vlt <- unique(util_rbind(data_frames_list = dups))
  }
  rownames(vlt) <- NULL
  vlt
}

.util_generate_value_label_table_name <- function(vl) {

  res <- util_parse_assignments(
    vl,
    split_on_any_split_char = FALSE,
    split_char = c(SPLIT_CHAR, '<'),
    multi_variate_text = TRUE)

  first_labs <- vapply(res, function(res_i) {
    suff <- ifelse(length(res_i) > 2, "...", "")
    res_i <- paste0(substr(head(res_i, 2), 1, 8), collapse = " ")
    paste0(res_i, suff)
  }, FUN.VALUE = character(1))

  hashes <- vapply(res, function(res_i) {
    hash <- substr(rlang::hash(res_i), 1, 5) # TODO: don't ignore hash collisions
  }, FUN.VALUE = character(1))

  res <- paste0("LABS_", first_labs, "::", hashes)
  return(res)
}

# TODO: Also work with only one sheet featuring columns CODE_LABEL, CODE_VALUE, CODE_ORDER and VALUE_LABEL_TABLE
