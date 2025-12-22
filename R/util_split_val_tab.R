#' Split table with mixed code/missing lists to single tables
#'
#' resulting tables are populated to the data frame cache.
#'
#' @param val_tab [data.frame] tables in one long data frame.
#'
#' @return `invisible(NULL)`
#'
#' @noRd
util_split_val_tab <- function(val_tab = CODE_LIST_TABLE) {
  util_expect_data_frame(val_tab, c(CODE_VALUE, CODE_CLASS))

  if (all(c(MISSING_LIST_TABLE, VALUE_LABEL_TABLE) %in% colnames(val_tab))) {
    if (length(setdiff(intersect(val_tab[[MISSING_LIST_TABLE]],
                  val_tab[[VALUE_LABEL_TABLE]]), NA)) > 0) {
      util_warning(c("Found the some table names",
                     "assigned as value and as well as as missing list table"),
                   applicability_problem = TRUE
                   )
    }
  }

  if (CODE_LIST_TABLE %in% colnames(val_tab)) {
    if (any(c(MISSING_LIST_TABLE, VALUE_LABEL_TABLE) %in% colnames(val_tab))) {
      util_warning("Have %s as well as %s or %s in %s, ignoring %s.",
                   sQuote(CODE_LIST_TABLE),
                   sQuote(MISSING_LIST_TABLE),
                   sQuote(VALUE_LABEL_TABLE),
                   dQuote(val_tab),
                   sQuote(CODE_LIST_TABLE),
                   applicability_problem = TRUE)
      val_tab[[CODE_LIST_TABLE]] <- NULL
    } else {
      vtn <- trimws(val_tab[[CODE_LIST_TABLE]])
      vtn <- vtn[!is.na(vtn)]
      val_tab[[MISSING_LIST_TABLE]] <- ""
      val_tab[[VALUE_LABEL_TABLE]] <- ""
      for (tbn in vtn) {
        which_mlt <- trimws(val_tab[[CODE_LIST_TABLE]]) == tbn &
          trimws(val_tab[[CODE_CLASS]]) %in% c("MISSING", "JUMP")
        if (any(which_mlt)) {
          val_tab[[MISSING_LIST_TABLE]][which_mlt] <- tbn
        }
        which_vlt <- trimws(val_tab[[CODE_LIST_TABLE]]) == tbn &
          trimws(val_tab[[CODE_CLASS]]) %in% c("VALUE")
        if (any(which_vlt)) {
          val_tab[[VALUE_LABEL_TABLE]][which_vlt] <- tbn
        }
      }
      val_tab[[CODE_LIST_TABLE]] <- NULL
    }
  }

  if (MISSING_LIST_TABLE %in% colnames(val_tab)) {
    mlt <- val_tab
    mlt[[MISSING_LIST_TABLE]][util_empty(mlt[[MISSING_LIST_TABLE]])] <-
      NA_character_
    mlts <- split(mlt, mlt[[MISSING_LIST_TABLE]])

    mlts <- mlts[vapply(mlts, FUN.VALUE = logical(1),
                        function(mlt) {
                          any(mlt[[CODE_CLASS]] %in% c(CODE_CLASSES$MISSING,
                                                       CODE_CLASSES$JUMP))
                        })]
    mlts <- lapply(mlts, `[[<-`, MISSING_LIST_TABLE, value = NULL)
    mlts <- lapply(mlts, `[[<-`, VALUE_LABEL_TABLE, value = NULL)
    mlts <- lapply(mlts, `[[<-`, CODE_ORDER, value = NULL) # TODO: Support code order also for missing tables
  }

  if (VALUE_LABEL_TABLE %in% colnames(val_tab)) {
    vlt <- val_tab
    vlt[[VALUE_LABEL_TABLE]][util_empty(vlt[[VALUE_LABEL_TABLE]])] <-
      NA_character_
    vlts <- split(vlt, vlt[[VALUE_LABEL_TABLE]])

    vlts <- vlts[vapply(vlts, FUN.VALUE = logical(1),
                        function(vlt) {
                          any(vlt[[CODE_CLASS]] %in% c(CODE_CLASSES$VALUE))
                        })]
    vlts <- lapply(vlts, `[[<-`, MISSING_LIST_TABLE, value = NULL)
    vlts <- lapply(vlts, `[[<-`, VALUE_LABEL_TABLE, value = NULL)
    vlts <- lapply(vlts, `[[<-`, CODE_CLASS, value = NULL)
    vlts <- lapply(vlts, `[[<-`, CODE_INTERPRET, value = NULL)
    vlts <- lapply(vlts, function(vlt) {
      if (CODE_ORDER %in% colnames(vlt)) {
        if (all(util_empty(vlt[[CODE_ORDER]]))) {
          vlt[[CODE_ORDER]] <- NULL
        }
      }
      vlt
    })
  }

  mixed_tab_names <- intersect(names(mlts), names(vlts))
  if (length(mixed_tab_names) > 0) {
    # have tables that have the same name but feature different content,
    # missing codes as well as value codes. need to disentangle these
    util_warning(
      c("Found code-list-tables that feature missing codes",
        "as well as value codes. This is not in line with dataquieR's",
        "metadata model, you should disentangle such tables. I'll add",
        "mixed tables, but this may cause problems."),
        applicability_problem = TRUE)
    # XXX
    vlts[mixed_tab_names] <- # overwrite the vlts with the combined version
      mapply(SIMPLIFY = FALSE,
           vlt = vlts[mixed_tab_names],
           mlt = mlts[mixed_tab_names],
           FUN = function(mlt, vlt) {
             util_rbind(mlt, vlt)
           })
  }

  prep_add_data_frames(data_frame_list = mlts)
  # write vlts after mlts, so the updated combined tables from XXX will
  # prevail
  prep_add_data_frames(data_frame_list = vlts)


  invisible(NULL)
}

#' Combine all missing and value lists to one big table
#'
#' @param meta_data [data.frame] item level meta data to be used, defaults to
#'   `"item_level"`
#' @param val_tab [character] name of the table being created: This table will
#'   be added to the data frame cache (or overwritten). If `NULL`, the table
#'   will only be returned
#'
#' @return [data.frame] the combined table
#'
#' @export
prep_unsplit_val_tabs <- function(meta_data = "item_level",
                                  val_tab = NULL) {
  util_expect_data_frame(meta_data)
  meta_data <- prep_meta_data_v1_to_item_level_meta_data(
    util_normalize_value_labels(meta_data))

  if (!VALUE_LABEL_TABLE %in% colnames(meta_data)) {
    meta_data[[VALUE_LABEL_TABLE]] <- ""
  }

  if (!MISSING_LIST_TABLE %in% colnames(meta_data)) {
    meta_data[[MISSING_LIST_TABLE]] <- ""
  }

  if (!is.null(val_tab) && val_tab %in% prep_list_dataframes()) {
    util_warning("Have already a table named %s, will overwrite this table.",
                 dQuote(val_tab))
  }

  clt <- c(meta_data[[VALUE_LABEL_TABLE]],
           meta_data[[MISSING_LIST_TABLE]])

  clt <- intersect(prep_list_dataframes(), clt)

  clts <- lapply(clt, prep_get_data_frame)

  tps <- lapply(clt, function(cltnm) {
    if (cltnm %in% meta_data[[VALUE_LABEL_TABLE]]) {
      return(VALUE_LABEL_TABLE)
    } else {
      return(MISSING_LIST_TABLE)
    }
  })

  clts <- mapply(tb = clts, nm = clt, tp = tps, FUN = function(tb, nm, tp) {
    tb[[tp]] <- nm
    if (tp == VALUE_LABEL_TABLE) {
      tb[[CODE_CLASS]] <- CODE_CLASSES$VALUE
    }
    tb
  })

  cltb <- util_rbind(data_frames_list = clts)

  if (!is.null(val_tab)) {

    data_frame_list <- list()

    data_frame_list[[val_tab]] <- cltb

    prep_add_data_frames(data_frame_list = data_frame_list)

  }

  invisible(util_attach_attr(cltb, meta_data = meta_data))
}

util_handle_val_tab <- function() {
  if (CODE_LIST_TABLE %in% prep_list_dataframes()) {
    util_split_val_tab(CODE_LIST_TABLE)
    rm(list = CODE_LIST_TABLE, envir = .dataframe_environment()) # TODO: Use resp. function from Elena, once available here from the dq_report_by-branch.
  }
}
