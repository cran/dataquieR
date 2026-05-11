#' Normalize and check cross-item-level metadata
#'
#' @param meta_data [meta_data]
#' @param meta_data_cross_item [cross-item-level metadata][meta_data_cross()]
#' @param label_col [character] label column to use for variable naming
#'
#' @return normalized and checked [cross-item-level metadata][meta_data_cross()]
#'
#' @family meta_data_cross
#' @seealso [meta_data_cross()]
#'
#' @noRd
util_normalize_cross_item <-
  function(
    meta_data = "item_level",
    meta_data_cross_item = "cross-item_level",
    label_col = LABEL) {
    # table of specified contradictions
    util_expect_data_frame(meta_data, list(
      VAR_NAMES = is.character
    ))
    util_expect_data_frame(meta_data_cross_item)
    if (!nrow(meta_data_cross_item)) {
      return(data.frame(
        VARIABLE_LIST = character(0),
        CHECK_LABEL = character(0),
        CONTRADICTION_TERM = character(0),
        CONTRADICTION_TYPE = character(0),
        MULTIVARIATE_OUTLIER_CHECKTYPE = character(0),
        N_RULES = integer(0),
        ASSOCIATION_RANGE = character(0),
        ASSOCIATION_METRIC = character(0),
        ASSOCIATION_DIRECTION = character(0),
        ASSOCIATION_FORM = character(0),
        REL_VAL = character(0),
        GOLDSTANDARD = character(0),
        CHECK_ID = character(0),
        DATA_PREPARATION = character(0)
      ))
    }

    if (!CONTRADICTION_TERM %in% colnames(meta_data_cross_item)) {
      meta_data_cross_item[[CONTRADICTION_TERM]] <- rep(NA_character_,
                                                    nrow(meta_data_cross_item))
    }

    # ensure unique identifiers for cross-item checks --------------------------
    if (!CHECK_ID %in% colnames(meta_data_cross_item)) { # as early as possible
      meta_data_cross_item[[CHECK_ID]] <- seq_len(nrow(meta_data_cross_item))
    }
    if (any(duplicated(meta_data_cross_item[[CHECK_ID]]))) {
      util_message(c("cross-item-level metadata: %s must be unique.",
                     "All IDs will be replaced"),
                   dQuote(CHECK_ID),
                   applicability_problem = TRUE)
      meta_data_cross_item[[CHECK_ID]] <- seq_len(nrow(meta_data_cross_item))
    }
    meta_data_cross_item[[CHECK_ID]] <- trimws(meta_data_cross_item[[CHECK_ID]])


    # Find all valid variable names:
    valid_name_cols <- eval(formals(util_find_var_by_meta)$allowed_sources,
                            envir = environment())
    valid_name_cols <- c(valid_name_cols,
                         VAR_NAMES,
                         label_col,
                         LABEL,
                         LONG_LABEL,
                         "ORIGINAL_VAR_NAMES",
                         "ORIGINAL_LABEL")
    lng <- getOption("dataquieR.lang", dataquieR.lang_default)
    if (nzchar(lng)) {
      valid_name_cols <- c(valid_name_cols,
                           paste0(valid_name_cols, "_", lng))
    }
    valid_name_cols <- intersect(unique(trimws(valid_name_cols)),
                                 colnames(meta_data))

    valid_names <- sort(unique(as.vector(unlist(meta_data[, valid_name_cols]))))

    # Expand CONTRADICTION_TERM using util_expand_pattern_rules
    # do before using the term for filling in variable_list
    expanded_contradiction_terms <-
      lapply(meta_data_cross_item[[CONTRADICTION_TERM]], function(rl) {
        r <- util_expand_pattern_rules(rl, valid_names = valid_names)
        if (length(r) == 0)
          r <- rl
        r
      })
    expanded_lengths <- lengths(expanded_contradiction_terms)
    expanded_indices <- sequence(expanded_lengths)
    expanded_source_rows <- rep(seq_len(nrow(meta_data_cross_item)),
                                expanded_lengths)

    meta_data_cross_item <-
      util_explode_var_names_lists(
        expanded_contradiction_terms,
        dframe = meta_data_cross_item,
        col_name = CONTRADICTION_TERM
      )

    expanded_clones <- expanded_lengths[expanded_source_rows] > 1L
    if (any(expanded_clones)) {
      original_check_ids <- meta_data_cross_item[[CHECK_ID]]
      meta_data_cross_item[[CHECK_ID]][expanded_clones] <- paste0(
        original_check_ids[expanded_clones],
        "_",
        expanded_indices[expanded_clones]
      )
      meta_data_cross_item[[CHECK_ID]] <- make.unique(
        meta_data_cross_item[[CHECK_ID]],
        sep = "_"
      )

      if (CHECK_LABEL %in% colnames(meta_data_cross_item)) {
        base_labels <- meta_data_cross_item[[CHECK_LABEL]]
        base_labels[util_empty(base_labels)] <- original_check_ids[
          util_empty(base_labels)]
        meta_data_cross_item[[CHECK_LABEL]][expanded_clones] <- paste0(
          base_labels[expanded_clones],
          " #",
          expanded_indices[expanded_clones]
        )
      }
    }

    if (VARIABLE_LIST %in% colnames(meta_data_cross_item)) {
      # Expand patterns in list column
      meta_data_cross_item[[VARIABLE_LIST]] <-
        unname(vapply(mapply(vl = meta_data_cross_item[[VARIABLE_LIST]],
                             id = meta_data_cross_item[[CHECK_ID]],
                             SIMPLIFY = FALSE,
                      function(vl, id) {
                        r <- util_expand_pattern_rules(
                          paste0("[",
                                 unlist(util_parse_assignments(
                                   vl,
                                   multi_variate_text = TRUE)),
                                 "]"),
                          valid_names = valid_names)
                        if (length(not_found <- attr(r, "mismatches")) > 0) {
                          util_warning(c("Entry VARIABLE_LIST for check %s",
                                         "maybe not specified correctly: %s"),
                                       paste(id),
                                       util_pretty_vector_string(
                                         as.vector(not_found)),
                                       applicability_problem = TRUE)

                        }
                        r <- gsub("^\\[(.*)\\]$", "\\1", trimws(r))
                        r <- paste(r, collapse = SPLIT_CHAR)
                        if (nzchar(r)) {
                          r
                        } else {
                          NA_character_
                        }
                      }), FUN = identity, FUN.VALUE = character(1)))
    }

    if (VARIABLE_LIST %in% colnames(meta_data_cross_item)) {
      meta_data_cross_item[[VARIABLE_LIST_ORDER]] <-
        meta_data_cross_item[[VARIABLE_LIST]]
    }

    # fill empty entries in VARIABLE_LIST and check for possible mismatches
    # between VARIABLE_LIST and CONTRADICTION_TERM -----------------------------
    if (!VARIABLE_LIST %in% colnames(meta_data_cross_item) ||
        any(util_empty(meta_data_cross_item[[VARIABLE_LIST]])) ||
        any(!util_empty(meta_data_cross_item[[CONTRADICTION_TERM]]))) {
      # generate VARIABLE_LIST entries from CONTRADICTION_TERM entries
      # TODO: Support "[" and "]" in variable labels
      needles_var_names <- unique(c(meta_data[[VAR_NAMES]],
                                    meta_data[[label_col]],
                                    meta_data[[LABEL]],
                                    meta_data[[LONG_LABEL]],
                                    meta_data[["ORIGINAL_VAR_NAMES"]],
                                    meta_data[["ORIGINAL_LABEL"]]))
      needles <- paste0("[",
                        needles_var_names,
                        "]")
      x <- vapply(setNames(needles, nm = needles_var_names),
                  grepl,
                  setNames(nm = meta_data_cross_item[[CONTRADICTION_TERM]]),
                  fixed = TRUE,
                  FUN.VALUE = logical(length = nrow(meta_data_cross_item))
      )
      if (is.vector(x)) {
        x <- as.matrix(t(x))
      }
      # variablelist_from_term <- lapply(lapply(lapply(apply(x, 1, which, simplify = FALSE), # apply supports simplify from R 4.1.0
      #                                      names), sort), unique)                # so use the following, less intuitive code line
      variablelist_from_term <- unname(lapply(as.data.frame(t(x)),
                                    function(xx) unique(sort(colnames(x)[xx]))))
      variablelist_from_term <-
        lapply(variablelist_from_term, paste0, collapse = sprintf(" %s ", SPLIT_CHAR))

      vl_from_term_empty <-
        vapply(variablelist_from_term, util_empty, FUN.VALUE = logical(1))

      if (!VARIABLE_LIST %in% colnames(meta_data_cross_item)) {
        meta_data_cross_item[[VARIABLE_LIST]] <- NA
      }

      # Empty VARIABLE_LIST entries can be replaced with the generated lists.
      vl_empty <- util_empty(meta_data_cross_item[[VARIABLE_LIST]])
      meta_data_cross_item[[VARIABLE_LIST]][vl_empty] <- variablelist_from_term[vl_empty]
      # For existing VARIABLE_LIST entries, we have to check whether they match
      # with the generated lists from CONTRADICTION_TERM. This will be done
      # below, after mapping the lists to `label_col`, because then both
      # lists can be compared more easily. We store the generated entries that
      # have to be checked in a temporary column.
      if (!all(seq_len(nrow(meta_data_cross_item)) %in% vl_empty)) {
        meta_data_cross_item[["VARIABLE_LIST_FROM_TERM"]] <- NA
        meta_data_cross_item[[
          "VARIABLE_LIST_FROM_TERM"]][!vl_empty & !vl_from_term_empty] <-
          variablelist_from_term[!vl_empty & !vl_from_term_empty]
      }
    }

    # parse variable lists to `label_col` to ensure uniform naming
    variablelist <- lapply(
        util_parse_assignments(meta_data_cross_item[[VARIABLE_LIST]],
                               multi_variate_text = TRUE
                               ), names)
    variablelist <- lapply(variablelist, function(x) util_find_var_by_meta(
      resp_vars = x,
      meta_data = meta_data,
      label_col = label_col,
      target = label_col,
      ifnotfound = NA_character_))
    # compare with lists from CONTRADICTION_TERM, if necessary
    if ("VARIABLE_LIST_FROM_TERM" %in% colnames(meta_data_cross_item)) {
      vl_from_term_filled <-
        !util_empty(meta_data_cross_item[["VARIABLE_LIST_FROM_TERM"]])
      variablelist_from_term <- lapply(util_parse_assignments(
        meta_data_cross_item[["VARIABLE_LIST_FROM_TERM"]][vl_from_term_filled],
        multi_variate_text = TRUE), names)
      variablelist_from_term <- lapply(variablelist_from_term, function(x) util_find_var_by_meta(
        resp_vars = x,
        meta_data = meta_data,
        label_col = label_col,
        target = label_col,
        ifnotfound = NA_character_))
      variablelist[vl_from_term_filled] <- mapply(
             vl = variablelist[vl_from_term_filled],
             vl_from_term = variablelist_from_term,
             id = meta_data_cross_item[[CHECK_ID]][vl_from_term_filled],
             SIMPLIFY = FALSE,
             FUN = function(vl, vl_from_term, id) { # TODO: Still needed after util_expand_pattern_rules
               not_in_vl <- vl_from_term[!(vl_from_term %in% vl)]
               not_in_vl_from_term <- vl[!(vl %in% vl_from_term)]
               if (length(not_in_vl) > 0 || length(not_in_vl_from_term) > 0) {
                 msg <- paste("Entry VARIABLE_LIST for check",
                              id,
                              "was not specified correctly and adapted:")
                 if (length(not_in_vl_from_term) > 0) {
                   msg <- paste(msg, "Variable(s)",
                                paste(not_in_vl_from_term, collapse = ", "),
                                ifelse(length(not_in_vl_from_term) > 1, "were", "was"),
                                "not used by the", sQuote(CONTRADICTION_TERM))
                 }
                 if (length(not_in_vl) > 0) {
                   if (length(not_in_vl_from_term) > 0) {
                     msg <- paste0(msg, ". Also,")
                   }
                   msg <- paste(msg, "Variable(s)",
                                paste(not_in_vl, collapse = ", "),
                                ifelse(length(not_in_vl) > 1, "were", "was"),
                                "missing from", sQuote(VARIABLE_LIST))
                 }
                 util_warning(msg, applicability_problem = FALSE)
                 # replace variable list with the generated one
                 return(vl_from_term)
               }
               return(vl)
             })
      # delete the temporary column
      meta_data_cross_item <-
        meta_data_cross_item[, -which(colnames(meta_data_cross_item) ==
                                        "VARIABLE_LIST_FROM_TERM")]
    }
    meta_data_cross_item[[VARIABLE_LIST]] <-
      lapply(variablelist, paste, collapse = sprintf(" %s ", SPLIT_CHAR))

    # ensure unique labels for the cross-item checks ---------------------------
    if (!CHECK_LABEL %in% colnames(meta_data_cross_item)) {
      meta_data_cross_item[[CHECK_LABEL]] <- meta_data_cross_item[[CHECK_ID]]
    }
    meta_data_cross_item[[CHECK_LABEL]][
      is.na(meta_data_cross_item[[CHECK_LABEL]])] <- paste0(
        "Check #",
        seq_along(meta_data_cross_item[[CHECK_LABEL]])[
          is.na(meta_data_cross_item[[CHECK_LABEL]])])
    if (any(duplicated(meta_data_cross_item[[CHECK_LABEL]]))) {
      util_message(c("Check labels cannot have duplicates in",
                   "cross-item_level metadata. I'll fix that"),
                   applicability_problem = TRUE)
      while(any(duplicated(meta_data_cross_item[[CHECK_LABEL]]))) {
        meta_data_cross_item[[CHECK_LABEL]][
          duplicated(meta_data_cross_item[[CHECK_LABEL]])] <- paste0(
            "Check #",
            seq_along(meta_data_cross_item[[CHECK_LABEL]])[
              duplicated(meta_data_cross_item[[CHECK_LABEL]])])

      }
    }

    # check DATA_PREPARATION ---------------------------------------------------
    set <- character(0)
    if ((VALUE_LABELS %in% colnames(meta_data) &&
         !all(util_empty(meta_data[[VALUE_LABELS]]))) ||
        (VALUE_LABEL_TABLE %in% colnames(meta_data) &&
         !all(util_empty(meta_data[[VALUE_LABEL_TABLE]]))) # ||
        # (STANDARDIZED_VOCABULARY_TABLE %in% colnames(meta_data) &&
        #  !all(util_empty(meta_data[[STANDARDIZED_VOCABULARY_TABLE]])))
        # FIXME: Enable for this case, too
    ) {
      set <- c(set, "LABEL")
    }

    set <- c(set, "MISSING_NA") # MISSING_LABEL MISSING_INTERPRET

    set <- c(set, "LIMITS")

    default_data_preparation <- paste(set, collapse = sprintf(" %s ",
                                                              SPLIT_CHAR))

    if (!DATA_PREPARATION %in% colnames(meta_data_cross_item)) {
      meta_data_cross_item[[DATA_PREPARATION]] <-
        rep(default_data_preparation, nrow(meta_data_cross_item))
    } else {
      meta_data_cross_item[[DATA_PREPARATION]][
        util_empty(meta_data_cross_item[[DATA_PREPARATION]])] <-
        rep(default_data_preparation,
            sum(util_empty(meta_data_cross_item[[DATA_PREPARATION]])))
    }

    meta_data_cross_item[[DATA_PREPARATION]] <- trimws(
      toupper(meta_data_cross_item[[DATA_PREPARATION]]))

    meta_data_cross_item[[DATA_PREPARATION]] <-
      gsub(
        "\\s+",
        " ",
        perl = TRUE,
        meta_data_cross_item[[DATA_PREPARATION]])

    unknown_tags <- lapply(lapply(
      util_parse_assignments(meta_data_cross_item[[DATA_PREPARATION]],
                             multi_variate_text = TRUE), names),
      FUN = function(x) { x[!(x %in%
                                c("LABEL",
                                  "MISSING_NA",
                                  "MISSING_LABEL",
                                  "MISSING_INTERPRET",
                                  "LIMITS" ))] } )

    meta_data_cross_item[[DATA_PREPARATION]] <- lapply(lapply(
      util_parse_assignments(meta_data_cross_item[[DATA_PREPARATION]],
                             multi_variate_text = TRUE), names),
      FUN = function(x) {
          tags <-
            x[(x %in%
                 c("LABEL",
                   "MISSING_NA",
                   "MISSING_LABEL",
                   "MISSING_INTERPRET",
                   "LIMITS" ))]
          if (any(duplicated(tags))) {
            util_message(
              c("Found duplicated %s tags in cross-item_level",
                "metadata cells. I'll ignore the duplicates."),
              sQuote(DATA_PREPARATION),
              applicability_problem = TRUE
            )
            tags <- unique(tags)
          }
          paste(tags, collapse = sprintf(" %s ", SPLIT_CHAR))
        }
    )

    meta_data_cross_item[[DATA_PREPARATION]][
      util_empty(meta_data_cross_item[[DATA_PREPARATION]])] <-
      sprintf(" %s ", SPLIT_CHAR)

    dupl_tags <- vapply(lapply(
      util_parse_assignments(meta_data_cross_item[[DATA_PREPARATION]],
                                 multi_variate_text = TRUE), names),
      FUN.VALUE = logical(1),
      FUN = function(x) { sum(startsWith(x,
          "MISSING_")) > 1 } )

    meta_data_cross_item$VARIABLE_LIST <-
      vapply(meta_data_cross_item$VARIABLE_LIST, paste, collapse = sprintf(" %s ", SPLIT_CHAR), FUN.VALUE = character(1))

    meta_data_cross_item$DATA_PREPARATION <-
      vapply(meta_data_cross_item$DATA_PREPARATION, paste, collapse = sprintf(" %s ", SPLIT_CHAR), FUN.VALUE = character(1))

    mapply(unknown_tags,
           paste0(
             "#",
             meta_data_cross_item[[CHECK_ID]],
             ": ",
             meta_data_cross_item[[CHECK_LABEL]],
             " (",
             meta_data_cross_item[[VARIABLE_LIST]],
             ")"
           ),
           SIMPLIFY = FALSE,
           FUN = function(tags, name) {
             if (length(tags) > 0)
               util_message(
                 c("In cross-item_level metadata, I found unknown %s tags: %s",
                   "Will ignore such tags."),
                 sQuote(DATA_PREPARATION),
                 util_pretty_vector_string(tags),
                 applicability_problem = TRUE)
           })

    if (any(dupl_tags)) {
      util_message(
        c("Found the following contradicting %s in %s:\n\n",
          "%s\n\nThese will be replaced by the default (%s",
          "default depends on %s),",
          "which may cause",
          "non-functional rules."),
        sQuote(DATA_PREPARATION),
        "cross-item_level metadata",
        paste0(
          "\t#",
          meta_data_cross_item[dupl_tags, CHECK_ID],
          ": ",
          meta_data_cross_item[dupl_tags, CHECK_LABEL],
          " (",
          meta_data_cross_item[dupl_tags, VARIABLE_LIST],
          "): ",
          meta_data_cross_item[dupl_tags, DATA_PREPARATION],
          collapse = "\n"
        ),
        default_data_preparation,
        "item_level metadata",
        applicability_problem = TRUE
      )
      meta_data_cross_item[dupl_tags, DATA_PREPARATION] <-
        default_data_preparation
    }

    # Check *_LIMITS
    for (lim in c(HARD_LIMITS, SOFT_LIMITS, DETECTION_LIMITS)) {
      if (lim %in% colnames(meta_data_cross_item)) {
        meta_data_cross_item[[lim]] <- vapply(
          meta_data_cross_item[[lim]],
          FUN.VALUE = character(1),
          FUN = function(rl) {
            rr <- withCallingHandlers(
              if (!util_empty(rl)) {
                if (length(util_parse_redcap_rule(rl)) > 0) {
                  rl
                } else {
                  NA_character_
                }
              } else {
                NA_character_
              },
              warning = function(cnd) {
                rl <- NA_character_
                m <- conditionMessage(cnd)
                util_warning("Cannot parse %s in cross item level: %s -- %s",
                             dQuote(lim),
                             sQuote(m),
                             dQuote(rl),
                             applicability_problem = TRUE
                             )
                invokeRestart("muffleWarning")
              }
            )
            rr
          }
        )
      }
    }

    # TODO: check the other columns
    meta_data_cross_item
}
