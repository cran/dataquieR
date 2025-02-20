#' Convert item-level metadata from v1.0 to v2.0
#'
#' This function is idempotent..
#'
#' The `options("dataquieR.force_item_specific_missing_codes")` (default
#' `FALSE`) tells the system, to always fill in `res_vars` columns to the
#' `MISSING_LIST_TABLE`, even, if the column already exists, but is empty.
#'
#' @param item_level [data.frame] the old item-level-metadata
#' @param meta_data [data.frame] old name for `item_level`
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param verbose [logical] display all estimated decisions, defaults to `TRUE`,
#'                          except if called in a [dq_report2] pipeline.
#' @param cause_label_df [data.frame] missing code table, see [cause_label_df].
#'                                    Optional. If this argument is given,
#'                                    you can add missing code tables.
#'
#' @return [data.frame] the updated metadata
#' @export
prep_meta_data_v1_to_item_level_meta_data <- function(item_level = "item_level",
                                                      verbose = TRUE,
                                                      label_col = LABEL,
                                                      cause_label_df,
                                                      meta_data = item_level) {

  # TODO: Rename, the function normalizes item_level metdata, not only in v1.0.
  # TOOD: Amend documentation: If you call it with v2 metadata, you can specify MISSING_LIST_TABLE; if so, then this table is searched using prep_get_dataframe, so it must have been loaded before,
  #        e.g. sing prep_load_workbook_like_file("inst/extdata/meta_data_extended.xlsx") or prep_add_data_frames(...). As a result, JUMP_LIST, MISSING_LIST and MISSING_LIST_TABLE will be consistent.
  #       JUMP_LIST and MISSING_LIST are used by all old functions to find information about jump codes and missing codes.
  #       MISSING_LIST_TABLE is compatible with the data frame passed as `cause_label_df` to `com_item_missingness`, so it works also with `prep_add_cause_label_df` and can be extracted form the JUMP_LIST and MISSING_LIST (without the v2 mapping columns for qualified mssingness) using `prep_extract_cause_label_df`
  # TODO: Re-Check, if besides prep_prepare_dataframes, all other a places, where meta_data is used, call this function

  if (missing(verbose) && .called_in_pipeline) {
    verbose <- FALSE
  }

  if (identical(attr(meta_data, "version"), 2)) {
    return(.util_internal_normalize_meta_data(meta_data, label_col))
  }

  util_expect_data_frame(meta_data,
                         col_names = list(VAR_NAMES = is.character),
                         convert_if_possible = list(VAR_NAMES = as.character))
  util_expect_scalar(verbose, check_type = is.logical)

  if (!LABEL %in% colnames(meta_data)) {
    meta_data[[LABEL]] <- meta_data[[VAR_NAMES]]
  }

  if (!label_col %in% colnames(meta_data)) {
    util_warning("Missing %s from %s.",
                 dQuote(label_col),
                 sQuote("meta_data"))
    meta_data[[label_col]] <- NA_character_
  }

  # End digits requested? ----
  if (DATA_ENTRY_TYPE %in% colnames(meta_data) &&
      END_DIGIT_CHECK %in% colnames(meta_data)) {
    util_warning("Have %s and %s in %s, will prefer %s.",
                 sQuote(END_DIGIT_CHECK),
                 sQuote(DATA_ENTRY_TYPE),
                 sQuote("meta_data"),
                 sQuote(END_DIGIT_CHECK))
    meta_data[[DATA_ENTRY_TYPE]] <- NULL
  }

  if (!END_DIGIT_CHECK %in% colnames(meta_data) &&
      DATA_ENTRY_TYPE %in% colnames(meta_data)) {
    meta_data[[END_DIGIT_CHECK]] <-
      !tolower(trimws(meta_data[[DATA_ENTRY_TYPE]])) %in% c("auto",
                                                            "automatic",
                                                            "machine",
                                                            "computational",
                                                            "electronic") &
      !util_is_na_0_empty_or_false(meta_data[[DATA_ENTRY_TYPE]])
    meta_data[[DATA_ENTRY_TYPE]] <- NULL
  }

  if (!END_DIGIT_CHECK %in% colnames(meta_data)) { # defaults to FALSE
    meta_data[[END_DIGIT_CHECK]] <- rep(FALSE, nrow(meta_data))
  }

  # KEY_STUDY_SEGMENT -> STUDY_SEGMENT and PART_VAR ----
  if (KEY_STUDY_SEGMENT %in% colnames(meta_data)) {
    if (!all(meta_data[[KEY_STUDY_SEGMENT]] %in% c(NA_character_,
                                                   meta_data[[VAR_NAMES]]))) {
      if (all(meta_data[[KEY_STUDY_SEGMENT]] %in% c(NA_character_,
                                                    meta_data[[label_col]]))) {
        util_message(
          c("All entries in %s are %s. Could convert them to",
            "%s."),
          dQuote(KEY_STUDY_SEGMENT),
          dQuote(label_col),
          dQuote(VAR_NAMES),
          applicability_problem = TRUE
        )
        meta_data[[KEY_STUDY_SEGMENT]] <-
          util_map_labels(meta_data[[KEY_STUDY_SEGMENT]],
                          meta_data,
                          to = VAR_NAMES,
                          from = label_col,
                          ifnotfound = meta_data[[KEY_STUDY_SEGMENT]])
      } else if (all(meta_data[[KEY_STUDY_SEGMENT]] %in% c(NA_character_,
                                                           meta_data[[LABEL]]
                                                           ))) {
        util_message(
          c("All entries in %s are %s. Could convert them to",
            "%s."),
          dQuote(KEY_STUDY_SEGMENT),
          dQuote(LABEL),
          dQuote(VAR_NAMES),
          applicability_problem = TRUE
        )
        meta_data[[KEY_STUDY_SEGMENT]] <-
          util_map_labels(meta_data[[KEY_STUDY_SEGMENT]],
                          meta_data,
                          to = VAR_NAMES,
                          from = LABEL,
                          ifnotfound = meta_data[[KEY_STUDY_SEGMENT]])
      } else {
        not_matched <- !(meta_data[[KEY_STUDY_SEGMENT]] %in%
                           c(NA_character_, meta_data[[VAR_NAMES]]))
        meta_data[[KEY_STUDY_SEGMENT]][not_matched] <-
          util_map_labels(meta_data[[KEY_STUDY_SEGMENT]][not_matched],
                          meta_data,
                          to = VAR_NAMES,
                          from = label_col,
                          ifnotfound = meta_data[[KEY_STUDY_SEGMENT]][not_matched])
        meta_data[[KEY_STUDY_SEGMENT]][not_matched] <-
          util_map_labels(meta_data[[KEY_STUDY_SEGMENT]][not_matched],
                          meta_data,
                          to = VAR_NAMES,
                          from = LABEL,
                          ifnotfound =
                            meta_data[[KEY_STUDY_SEGMENT]][not_matched])
        not_matched <- !(meta_data[[KEY_STUDY_SEGMENT]] %in%
                           c(NA_character_, meta_data[[VAR_NAMES]]))
        util_message(
          c("Not all entries in %s are found in %s. Could convert %d of",
            "these %d to match %s. This may be caused by providing",
            "subsets of %s."),
          dQuote(KEY_STUDY_SEGMENT),
          dQuote(VAR_NAMES),
          length(not_matched) - sum(not_matched, na.rm = TRUE),
          length(not_matched),
          dQuote(VAR_NAMES),
          dQuote("meta_data"),
          applicability_problem = TRUE
        )
      }
    }
  }

  if (KEY_STUDY_SEGMENT %in% colnames(meta_data)) {
    if (all(meta_data[[KEY_STUDY_SEGMENT]] %in%
            c(meta_data[[VAR_NAMES]], NA_character_))) {
      # KEY_STUDY_SEGMENT is PART_VAR, new STUDY_SEGMENT is the LABEL of the
      # referred variable
      if (verbose)
        util_message("%s is %s, new %s is the %s of the referred variable",
                     sQuote(KEY_STUDY_SEGMENT),
                     sQuote(PART_VAR),
                     sQuote(STUDY_SEGMENT),
                     sQuote(LABEL))
      meta_data[[PART_VAR]] <- meta_data[[KEY_STUDY_SEGMENT]]
      meta_data[[STUDY_SEGMENT]] <-
        util_map_labels(meta_data[[KEY_STUDY_SEGMENT]],
                        meta_data = meta_data,
                        warn_ambiguous = FALSE,
                        ifnotfound = meta_data[[KEY_STUDY_SEGMENT]])
      meta_data[[KEY_STUDY_SEGMENT]] <- NULL
    } else if (!any(meta_data[[KEY_STUDY_SEGMENT]] %in%
                    c(NA_character_, meta_data[[VAR_NAMES]]))) {
      # KEY_STUDY_SEGMENT is STUDY_SEGMENT, PART_VAR is empty
      if (verbose)
        util_message("%s is %s, %s is empty",
                     sQuote(KEY_STUDY_SEGMENT),
                     sQuote(STUDY_SEGMENT),
                     sQuote(PART_VAR))
      meta_data[[STUDY_SEGMENT]] <-
        meta_data[[KEY_STUDY_SEGMENT]]
      meta_data[[KEY_STUDY_SEGMENT]] <- NULL
    } else {
      util_error(c("Some, but not all entries in %s found in %s. So, I cannot",
                   "decide, if %s contains links to %s or labels for %s only."),
                 sQuote(KEY_STUDY_SEGMENT), sQuote(VAR_NAMES),
                 sQuote(KEY_STUDY_SEGMENT), sQuote(PART_VAR),
                 sQuote(STUDY_SEGMENT))
    }
  }

  if (PART_VAR %in% colnames(meta_data)) {
    if (!all(meta_data[[PART_VAR]] %in% c(NA_character_,
                                          meta_data[[VAR_NAMES]]))) {
      if (all(meta_data[[PART_VAR]] %in% c(NA_character_,
                                           meta_data[[label_col]]))) {
        # util_message(
        #   c("All entries in %s (old: %s) are %s. Could convert them to",
        #     "standard, i.e., %s."),
        #   dQuote(PART_VAR),
        #   dQuote(KEY_STUDY_SEGMENT),
        #   dQuote(label_col),
        #   dQuote(VAR_NAMES),
        #   applicability_problem = TRUE
        # )
        meta_data[[PART_VAR]] <-
          util_map_labels(meta_data[[PART_VAR]],
                          meta_data,
                          to = VAR_NAMES,
                          from = label_col,
                          ifnotfound = meta_data[[PART_VAR]])
      } else if (all(meta_data[[PART_VAR]] %in% c(NA_character_,
                                                  meta_data[[LABEL]]))) {
        # TODO: Be robust, if LABEL, LONG_LABEL, VAR_NAMES and label_col are mixed
        # util_message(
        #   c("All entries in %s (old: %s) are %s. Could convert them to",
        #     "standard, i.e., %s."),
        #   dQuote(PART_VAR),
        #   dQuote(KEY_STUDY_SEGMENT),
        #   dQuote(LABEL),
        #   dQuote(VAR_NAMES),
        #   applicability_problem = TRUE
        # )
        meta_data[[PART_VAR]] <-
          util_map_labels(meta_data[[PART_VAR]],
                          meta_data,
                          to = VAR_NAMES,
                          from = LABEL,
                          ifnotfound = meta_data[[PART_VAR]])
      } else {
        not_matched <- !(meta_data[[PART_VAR]] %in% c(NA_character_,
                                                      meta_data[[VAR_NAMES]]))
        meta_data[[PART_VAR]][not_matched] <-
          util_map_labels(meta_data[[PART_VAR]][not_matched],
                          meta_data,
                          to = VAR_NAMES,
                          from = label_col,
                          ifnotfound = meta_data[[PART_VAR]][not_matched])
        meta_data[[PART_VAR]][not_matched] <-
          util_map_labels(meta_data[[PART_VAR]][not_matched],
                          meta_data,
                          to = VAR_NAMES,
                          from = LABEL,
                          ifnotfound = meta_data[[PART_VAR]][not_matched])
        not_matched <- !(meta_data[[PART_VAR]] %in%
                           c(NA_character_, meta_data[[VAR_NAMES]]))
        util_message(
          c("Not all entries in %s (old: %s) are found in %s.",
            "Could convert %d of",
            "these %d to match %s. This may be caused by providing",
            "subsets of %s."),
          dQuote(PART_VAR),
          dQuote(KEY_STUDY_SEGMENT),
          dQuote(VAR_NAMES),
          length(not_matched) - sum(not_matched, na.rm = TRUE),
          length(not_matched),
          dQuote(VAR_NAMES),
          dQuote("meta_data"),
          applicability_problem = TRUE
        )
      }
    }
  }

  # KEY_DATETIME -> TIME_VAR ----
  if (KEY_DATETIME %in% colnames(meta_data)) {
    meta_data[[TIME_VAR]] <- meta_data[[KEY_DATETIME]]
    meta_data[[KEY_DATETIME]] <- NULL
  }

  # GROUP_VAR_OBSERVER, GROUP_VAR_DEVICE -- KEY_* -> GROUP_VAR_* ----
  colnames(meta_data) <- gsub("KEY_(.*)",
                             "GROUP_VAR_\\1",
                             perl = TRUE,
                             colnames(meta_data))

  if (!missing(cause_label_df)) {
    util_expect_data_frame(cause_label_df,
                           c(CODE_VALUE, CODE_LABEL))
    meta_data[] <-
      prep_add_cause_label_df(meta_data = meta_data,
                              cause_label_df = cause_label_df,
                              label_col = VAR_NAMES)
  }

  attr(meta_data, "version") <- 2

  return(.util_internal_normalize_meta_data(meta_data, label_col = label_col))
}


#' Make normalizations of v2.0 item_level metadata.
#'
#' Requires referred missing-tables being available by
#' `prep_get_data_frame`.
#'
#' @inheritParams prep_meta_data_v1_to_item_level_meta_data
#' @keywords internal
.util_internal_normalize_meta_data <- function(meta_data = "item_level",
                                               label_col = LABEL,
                                               verbose = TRUE) {
  if (!identical(attr(meta_data, "version"), 2)) {
    util_error(c(
      "Internal error: .util_internal_normalize_meta_data called on meta_data",
      "below v2.0"))
  }

  if (VALUE_LABELS %in% names(meta_data)) {
    # this should be checked outside  (!identical(attr(meta_data,
    # "normalized"), TRUE)), because the user could have modified processed
    # metadata adding a column VALUE_LABELS in between, so the data claims to
    # be normalized, but it isn't any more. However, the existence of the
    # column VALUE_LABELS will always tell us, if we have something to
    # do, here. Maybe, we need to use check-sums for such attributes that
    # express the grade of normalization of an object?
    meta_data <- util_normalize_value_labels(meta_data = meta_data)
  }

  already_warned <- c()
  already_warned_env <- environment()

  if (!identical(attr(meta_data, "normalized"), TRUE)) {

    if (!N_RULES %in% names(meta_data)) {
      meta_data[[N_RULES]] <- NA_integer_
    }

    if (!UNIVARIATE_OUTLIER_CHECKTYPE %in% names(meta_data)) {
      meta_data[[UNIVARIATE_OUTLIER_CHECKTYPE]] <- NA_character_
    }

    if (is.null(meta_data[[MISSING_LIST_TABLE]])) {
      meta_data[[MISSING_LIST_TABLE]] <- rep(NA_character_, nrow(meta_data))
    }
    no_mlt <- util_empty(meta_data[[MISSING_LIST_TABLE]])
    has_both_mlt_and_ml_jl <- !no_mlt &
      (!util_empty(meta_data[[JUMP_LIST]]) |
         !util_empty(meta_data[[MISSING_LIST]]))
    if (any(has_both_mlt_and_ml_jl)) { # have to combine lists
      util_warning(c("Should not have %s and %s / %s for the same variable.",
                     "I'll try to combine them. This will take some time, so",
                     "you should fix this in your %s to avoid waiting."),
                   dQuote(MISSING_LIST_TABLE),
                   dQuote(MISSING_LIST),
                   dQuote(JUMP_LIST),
                   sQuote("meta_data"),
                   applicability_problem = TRUE)
      for (r in which(has_both_mlt_and_ml_jl)) {
        tb <- data.frame(CODE_VALUE = character(0),
                         CODE_LABEL = character(0),
                         CODE_INTERPRET = character(0))
        try({
          tb <- prep_get_data_frame(meta_data[r, MISSING_LIST_TABLE])
          if (!CODE_VALUE %in% colnames(tb)) {
            tb[[CODE_VALUE]] <- NA
          }
          tb <- tb[!util_empty(tb[[CODE_VALUE]]), ,
                                 FALSE]
          if (CODE_INTERPRET %in% colnames(tb) &&
              !CODE_CLASS %in% colnames(tb)) {
            tb[[CODE_CLASS]] <- ifelse(tb[[CODE_INTERPRET]] %in%
                                           c("NE"), "JUMP", "MISSING") # TODO: Verify, if, e.g., PL is also a JUMP
          }
        })
        legacy_tb <- data.frame(CODE_VALUE = character(0),
                                CODE_LABEL = character(0))
        try({
          legacy_tb <- prep_extract_cause_label_df(
            meta_data[r, , drop = FALSE], label_col = label_col)$cause_label_df
          util_stop_if_not(length(unique(legacy_tb$resp_vars)) < 2)
          legacy_tb$resp_vars <- NULL # is always only for the item in row r
          if (!CODE_VALUE %in% colnames(legacy_tb)) {
            legacy_tb[[CODE_VALUE]] <- NA
          }
          legacy_tb <- legacy_tb[!util_empty(legacy_tb[[CODE_VALUE]]), ,
                                 FALSE]
        })
        cld_name <- paste0(meta_data[r, MISSING_LIST_TABLE], '_',
                           prep_link_escape(meta_data[r, label_col],
                                            html = TRUE))
        if (exists( # find an available name for a new data frame
          cld_name,
          envir = .dataframe_environment())) {
          i <- 0
          while (exists( # find an available name for a new data frame
            paste0(cld_name, "_", i),
            envir = .dataframe_environment())) i <- i + 1
          cld_name <- paste0(cld_name, "_", i)
        }
        meta_data[r, MISSING_LIST] <- NA
        meta_data[r, JUMP_LIST] <- NA
        meta_data[r, MISSING_LIST_TABLE] <- NA
        combined_table <- merge(legacy_tb, tb, by = CODE_VALUE, all = TRUE)
        if (all(c("CODE_LABEL.x", "CODE_LABEL.y") %in%
                colnames(combined_table))) {
          combined_table[util_empty(combined_table$CODE_LABEL.x) |
                           combined_table$CODE_LABEL.x ==
            paste("MISSING", combined_table$CODE_VALUE), "CODE_LABEL.x"] <-
            combined_table[util_empty(combined_table$CODE_LABEL.x) |
                             combined_table$CODE_LABEL.x ==
                             paste("MISSING", combined_table$CODE_VALUE),
                           "CODE_LABEL.y"]

          combined_table[util_empty(combined_table$CODE_LABEL.x) |
                           combined_table$CODE_LABEL.x ==
            paste("JUMP", combined_table$CODE_VALUE), "CODE_LABEL.x"] <-
            combined_table[util_empty(combined_table$CODE_LABEL.x) |
                             combined_table$CODE_LABEL.x ==
                             paste("JUMP", combined_table$CODE_VALUE),
                           "CODE_LABEL.y"]

          combined_table[util_empty(combined_table$CODE_LABEL.y) |
                           combined_table$CODE_LABEL.y ==
            paste("MISSING", combined_table$CODE_VALUE), "CODE_LABEL.y"] <-
            combined_table[util_empty(combined_table$CODE_LABEL.y) |
                             combined_table$CODE_LABEL.y ==
                             paste("MISSING", combined_table$CODE_VALUE),
                           "CODE_LABEL.x"]

          combined_table[util_empty(combined_table$CODE_LABEL.y) |
                           combined_table$CODE_LABEL.y ==
            paste("JUMP", combined_table$CODE_VALUE), "CODE_LABEL.y"] <-
            combined_table[util_empty(combined_table$CODE_LABEL.y) |
                             combined_table$CODE_LABEL.y ==
                             paste("JUMP", combined_table$CODE_VALUE),
                           "CODE_LABEL.x"]
          combined_table$CODE_LABEL <- combined_table$CODE_LABEL.x
          combined_table$CODE_LABEL.x <- NULL
          combined_table$CODE_LABEL.y <- NULL
        }
        if (all(c("CODE_CLASS.x", "CODE_CLASS.y") %in%
                colnames(combined_table))) {
          combined_table[util_empty(combined_table$CODE_CLASS.x),
                         "CODE_CLASS.x"] <-
            combined_table[util_empty(combined_table$CODE_CLASS.x),
                           "CODE_CLASS.y"]
          combined_table[util_empty(combined_table$CODE_CLASS.y),
                         "CODE_CLASS.y"] <-
            combined_table[util_empty(combined_table$CODE_CLASS.y),
                           "CODE_CLASS.x"]
          if (any(
            (!is.na(combined_table$CODE_CLASS.y) &
             combined_table$CODE_CLASS.x != combined_table$CODE_CLASS.y))) {
            util_warning(
              c("Code classes in old and new missing code settings",
                "don't match for %s, missing and jump codes overlap.",
                "Will use the new %s: %s as the reference."),
              dQuote(meta_data[r, label_col]),
              sQuote(MISSING_LIST_TABLE),
              dQuote(cld_name),
              applicability_problem = TRUE)
          }
          combined_table$CODE_CLASS <- combined_table$CODE_CLASS.y
          combined_table$CODE_CLASS.x <- NULL
          combined_table$CODE_CLASS.y <- NULL
        }
        if (prod(dim(combined_table))) {
          l <- list()
          l[[cld_name]] <- combined_table
          prep_add_data_frames(data_frame_list = l)
          # assign the new cause label df as MISSING_LIST_TABLE
          meta_data[r, MISSING_LIST_TABLE] <- cld_name
        }
      }
    }
    if (any(no_mlt)) {
      # If no missing list table, yet, for a variable
      # generate one from JUMP_LIST/MISSING_LIST
      cld <- prep_extract_cause_label_df(
        meta_data = meta_data[no_mlt, , FALSE],
        label_col = VAR_NAMES)
      meta_data[no_mlt, colnames(cld$meta_data)] <- cld$meta_data
      i <- 0
      while (exists( # find an available name for a new data frame
        paste0(MISSING_LIST_TABLE, "_", i),
        envir = .dataframe_environment())) i <- i + 1
      cld_name <- paste0(MISSING_LIST_TABLE, "_", i)

      # store in the internal data frame cache
      l <- list()

      if (prod(dim(cld$cause_label_df))) {
        l[[cld_name]] <- cld$cause_label_df
        prep_add_data_frames(data_frame_list = l)

        # assign the new cause label df as MISSING_LIST_TABLE
        meta_data[no_mlt, MISSING_LIST_TABLE] <- cld_name
      }
    }

    mlts <- mapply(
      SIMPLIFY = FALSE,
      setNames(meta_data[[MISSING_LIST_TABLE]], nm = meta_data[[VAR_NAMES]]),
      meta_data[[VAR_NAMES]],
      FUN = function(cld, vn) {
        if (all(is.na(cld))) {
          return(NULL)
        }
        r <- data.frame()
        try(silent = TRUE,
          withCallingHandlers(
            r <- prep_get_data_frame(cld),
          error = function(cnd) {
            if (!isTRUE(already_warned_env$already_warned[cld])) {
              already_warned_env$already_warned[cld] <- TRUE
              my_cnd <- warningCondition(
                sprintf(
                  "Could not load table %s: %s",
                  dQuote(cld),
                  sQuote(sub("\nwhen calling .*$", "", conditionMessage(cnd)))
                )
              )
              util_warning(my_cnd, immediate = TRUE) # TODO: Check for sheet names very close to standard names, warn in such cases.
            }
          })
        )
        if (!prod(dim(r))) return(data.frame())
        if (!("resp_vars" %in% colnames(r))) {
          r$resp_vars <- vn
        } else {
          have_vn <- !util_empty(r$resp_vars)
          mapped_vn <- r$resp_vars[have_vn]
          other_col <- intersect(c(VAR_NAMES, LABEL, LONG_LABEL),
                                 colnames(meta_data)) # TODO: For all similar mapping functions (util_find_var_by_meta and util_correct_variable_use)
          map_res <- lapply(setNames(nm = other_col), function(oc) {
            unname(
              util_map_labels(
                mapped_vn,
                warn_ambiguous = FALSE,
                meta_data = meta_data,
                to = VAR_NAMES,
                from = oc,
                ifnotfound = mapped_vn
              )
            )
          })
          map_res <- as.data.frame(t(as.data.frame(map_res)))
          mapped_vn <-
            vapply(
              FUN.VALUE = character(1),
              map_res,
              FUN = function(mr) {
                mr <- mr[!is.na(mr)]
                mr <- mr[[1]]
                as.character(mr)
                # unique(na.omit(as.character(map_res[i, ])))[1]
                # We select here the first element, such that in case of ambiguities
                # VAR_NAME is preferred over LABEL and LONG_LABEL, and LABEL over LONG_LABEL.
                # If there are no ambiguities, this works as well.
              }
            )
          r$resp_vars[have_vn] <- mapped_vn
          if (isTRUE(getOption("dataquieR.force_item_specific_missing_codes",
                               FALSE))) {
            r$resp_vars[!have_vn] <- vn
          }
          r
        }
        r[util_empty(r$resp_vars) | r$resp_vars == vn, , FALSE]
      }
    )

    mlts <- lapply(mlts, function(mlt) { # avoid coerction of missing code values
      if (CODE_VALUE %in% colnames(mlt))
        mlt[[CODE_VALUE]] <- as.character(mlt[[CODE_VALUE]])
      mlt
    })

    mlts <- util_rbind(data_frames_list = mlts)

    # Ensure, that we now only have variable specific cause_label_df rows
    if (prod(dim(mlts))) {
      util_stop_if_not(!is.null(mlts$resp_vars))
      util_stop_if_not(all(!util_empty(mlts$resp_vars)))

      # ensure, that all missing codes for an item are in MISSING/JUMP_LIST
      new <- prep_add_cause_label_df(meta_data = meta_data,
                                     cause_label_df = mlts,
                                     label_col = VAR_NAMES)

      new_cols <- setdiff(colnames(new), colnames(meta_data))
      if (length(new_cols)) {
        meta_data[, new_cols] <- NA
      }

      lost_cols <- setdiff(colnames(meta_data), colnames(new))
      if (length(lost_cols)) {
        new[, lost_cols] <- NA
      }

      util_stop_if_not(length(intersect(colnames(meta_data), colnames(new))) ==
                         length(union(colnames(meta_data), colnames(new))))

      if (!all(colnames(meta_data) == colnames(new))) {
        new <- new[, colnames(meta_data)]
      }

      meta_data[] <- new
    }
    attr(meta_data, "normalized") <- TRUE
  }
  meta_data
}

# TODO: add some metadata management functios (write to XLSX, edit, ...)
