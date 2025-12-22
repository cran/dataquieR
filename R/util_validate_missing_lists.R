#' Validate code lists for missing and/or jump codes
#'
#' will warn/stop on problems
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame can be
#'                                    specified here, see [cause_label_df]
#' @param assume_consistent_codes [logical] if TRUE and no labels are given and
#'                                          the same missing/jump code is used
#'                                          for more than one variable, the
#'                                          labels assigned for this code will
#'                                          be the same for all variables.
#' @param expand_codes [logical] if TRUE, code labels are copied from other
#'                               variables, if the code is the same and the
#'                               label is set somewhere
#' @param suppressWarnings [logical] warn about consistency issues with missing
#'                                   and jump lists
#'
#'
#' @return [list] with entries:
#'   - `cause_label_df` updated data frame with labels for missing codes
#'
#' @family metadata_management
#' @concept robustness
#' @noRd


util_validate_missing_lists <-
  function(meta_data, cause_label_df,
           assume_consistent_codes = FALSE,
           expand_codes = FALSE,
           suppressWarnings = FALSE,
           label_col) {
    if (missing(label_col)) {
      label_col <- WELL_KNOWN_META_VARIABLE_NAMES$VAR_NAMES
    }
    if (missing(meta_data) ||
        !is.data.frame(meta_data) ||
        !(all(c(MISSING_LIST, label_col) %in% colnames(meta_data)))) {
      util_error("Need at least %s with columns %s and %s",
                 dQuote("meta_data"),
                 dQuote(MISSING_LIST),
                 dQuote(label_col),
                 applicability_problem = TRUE)
    }

    if (!(JUMP_LIST %in% colnames(meta_data))) {
      meta_data[[JUMP_LIST]] <- NA_character_
    }

    cause_label_df_given <- !missing(cause_label_df)

    if (!cause_label_df_given) {
      cause_label_df <- data.frame(
        CODE_VALUE = NA,
        CODE_LABEL = NA,
        stringsAsFactors = FALSE
      )[FALSE, , FALSE]
    }

    not_empty <- function(x) {
      !is.na(x) & trimws(gsub(SPLIT_CHAR, "", x, fixed = TRUE)) != ""
    }

    is_assignment <- function(x) {
      grepl("=", x, fixed = TRUE)
    }

    if (!suppressWarnings && cause_label_df_given &&
        (
          any(is_assignment(meta_data$JUMP_LIST)) ||
          any(is_assignment(meta_data$MISSING_LIST))
        )) {
      util_warning(c("Combining %s and assignments in %s and/or %s",
                     "in %s is discouraged. This may cause errors."),
                   dQuote("cause_label_df"),
                   dQuote(MISSING_LIST),
                   dQuote(JUMP_LIST),
                   dQuote("meta_data"),
                   applicability_problem = TRUE)
    }

    if (!suppressWarnings &&
        (
          any(is_assignment(meta_data$JUMP_LIST)) ||
          any(is_assignment(meta_data$MISSING_LIST))
        ) &&
        (
          any(not_empty(meta_data$JUMP_LIST) &
             !is_assignment(meta_data$JUMP_LIST)) ||
         any(not_empty(meta_data$MISSING_LIST) &
             !is_assignment(meta_data$MISSING_LIST))
        )
      ) {
      util_warning(c("%s and assignment notation in %s or %s in %s have been",
                     "combined. This is discouraged",
                     "and may cause unexpected results. Please use %s to add",
                     "all entries from %s to %s"),
                   dQuote("cause_label_df"),
                   dQuote(MISSING_LIST),
                   dQuote(JUMP_LIST),
                   dQuote("meta_data"),
                   dQuote("prep_add_cause_label_df"),
                   dQuote("cause_label_df"),
                   dQuote("meta_data"),
                   applicability_problem = TRUE)
    }

    if (!is.data.frame(cause_label_df) ||
        !(all(c(CODE_VALUE, CODE_LABEL) %in% colnames(cause_label_df)))) {
      util_message(
        c("Need columns %s in %s, which must be a data frame if given.",
          "Will ignore this argument"),
        paste(dQuote(c(CODE_VALUE, CODE_LABEL)), collapse = ", "),
        dQuote("cause_label_df"),
        applicability_problem = TRUE)
      cause_label_df <- data.frame(
        CODE_VALUE = NA,
        CODE_LABEL = NA,
        stringsAsFactors = FALSE
      )[FALSE, , FALSE]
    }

    if (CODE_CLASS %in% colnames(cause_label_df)) {
      if (!suppressWarnings &&
          !all(cause_label_df$CODE_CLASS %in% c("MISSING", "JUMP", NA))) {
        util_error("Only %s and %s are valid values for the column %s in %s.",
                   dQuote("MISSING"),
                   dQuote("JUMP"),
                   dQuote(CODE_CLASS),
                   dQuote("cause_label_df"),
                   applicability_problem = TRUE)
      }
    }

    if (!suppressWarnings &&
       (any(is.na(cause_label_df$CODE_VALUE)) ||
        any(is.na(cause_label_df$CODE_LABEL)))) {
      util_warning("Some code labels or -values are missing from %s.",
                   dQuote("cause_label_df"),
                   applicability_problem = TRUE)
    }

    parsed_lists_md <-  unlist(
      util_parse_assignments(c(meta_data$JUMP_LIST,
                               meta_data$MISSING_LIST),
                               multi_variate_text = TRUE),
      recursive = TRUE)
    parsed_lists_cldf <- setNames(cause_label_df$CODE_LABEL,
                     nm = cause_label_df$CODE_VALUE)
    parsed_lists <- c(parsed_lists_md,
                      parsed_lists_cldf)
    parsed_lists <- parsed_lists[!is.na(parsed_lists)]

    not_numbers <-
      (
         is.na(suppressWarnings(as.numeric(names(parsed_lists)))) &
         suppressWarnings(vapply(lapply(
           names(parsed_lists), util_parse_date), is.na,
                FUN.VALUE = logical(1))) &
           suppressWarnings(vapply(lapply(
             names(parsed_lists), util_parse_time), is.na,
             FUN.VALUE = logical(1)))
      ) !=
      (is.na(names(parsed_lists)) |
      trimws(names(parsed_lists)) == "" |
      trimws(names(parsed_lists)) == "NA")

    if (!suppressWarnings && any(not_numbers)) {
      util_warning(c("Some missing codes are not numeric or date/time. This is",
                     "not supported yet: %s"),
                   paste(dQuote(names(parsed_lists[not_numbers])),
                         collapse = ", "),
                   applicability_problem = TRUE)
    }

    if (cause_label_df_given) {
      not_in_cldf <- sort(unique(names(parsed_lists_md)[!(names(parsed_lists_md) %in% c("NA", cause_label_df$CODE_VALUE))]))
      if (!suppressWarnings && length(not_in_cldf) > 0) {
        util_warning("Found jump/missing codes in %s not mentioned in %s: %s",
                     dQuote("meta_data"),
                     dQuote("cause_label_df"),
                     paste(sQuote(not_in_cldf), collapse = ", "))
      }

      not_in_md <- unique(sort(names(parsed_lists_cldf)[!(names(parsed_lists_cldf) %in% c("NA", names(parsed_lists_md)))]))
      if (!suppressWarnings && length(not_in_md) > 0) {
        util_warning("Found jump/missing codes in %s not mentioned in %s: %s",
                     dQuote("cause_label_df"),
                     dQuote("meta_data"),
                     paste(sQuote(not_in_md), collapse = ", "))
      }
    }

    parsed_lists <- parsed_lists[!not_numbers &
                                   trimws(names(parsed_lists)) != ""]

    # not_named <- parsed_lists[
    #   as.numeric(names(parsed_lists)) == parsed_lists]
    #
    # if (!suppressWarnings &&
    #     length(not_named)) {
    #   util_warning("Some code labels or -values are not named: %s.",
    #                paste(sQuote(unique(not_named)),
    #                  collapse = ", "),
    #                applicability_problem = TRUE)
    # }

    if (!"resp_vars" %in% colnames(cause_label_df)) {
      nm <- meta_data[, label_col, FALSE]
      colnames(nm) <- "resp_vars"
      cause_label_df <- merge(cause_label_df,
                              nm,
                              by = c(),
                              all = TRUE)
      if (colnames(cause_label_df)[ncol(cause_label_df)] == "y[FALSE, ]") {
        colnames(cause_label_df)[ncol(cause_label_df)] <- "resp_vars"
      }
    }

    if (!CODE_CLASS %in% colnames(cause_label_df)) {
      cause_label_df$CODE_CLASS <- rep("MISSING", nrow(cause_label_df))
    }

    cldf2 <- prep_extract_cause_label_df(meta_data = meta_data,
                                         label_col = label_col)$cause_label_df

    if (!!prod(dim(cause_label_df)) &&
        !!prod(dim(cldf2)))
    cause_label_df <- util_rbind(
      cause_label_df,
      cldf2
    ) else if (!prod(dim(cause_label_df)) &&
               !!prod(dim(cldf2))) {
      cause_label_df <- cldf2
    }

    cause_label_df <- cause_label_df[!duplicated(cause_label_df), , FALSE]

    vars_with_dups <-
      vapply(split(cause_label_df,
                   cause_label_df$resp_vars),
             function(x) {
               x <- x[!duplicated(x), , FALSE]
               y <- x$CODE_VALUE
               anyDuplicated(y) > 0
             },
             FUN.VALUE = logical(1))

    if (!suppressWarnings && any(vars_with_dups)) {
      util_warning(
        "Found at least one missing code with more than one meaning for %s.",
        paste(dQuote(names(vars_with_dups[vars_with_dups])), collapse = ", "),
        applicability_problem = TRUE
        )
    }

    if (assume_consistent_codes) {
      cld <-
        cause_label_df[, c(CODE_VALUE, CODE_LABEL, CODE_CLASS), FALSE]
      cld <- cld[!duplicated(cld), , FALSE]
      d <- duplicated(cld$CODE_VALUE)
      if (any(d)) {
        dd <- unique(cld[d, CODE_VALUE, TRUE])
        if (!suppressWarnings) {
          util_warning(
            "Found missing code(s) with more than one meaning:\n%s",
            paste(capture.output(
              print(cld[cld$CODE_VALUE %in% dd, , FALSE])),
              collapse = "\n"),
            applicability_problem = TRUE
          )
        }
      }
    }

    if (assume_consistent_codes) {
      cause_label_df$AUTO <- cause_label_df$CODE_LABEL ==
        paste(cause_label_df$CODE_CLASS, cause_label_df$CODE_VALUE)
    } else {
      cause_label_df$AUTO <- cause_label_df$CODE_LABEL ==
        paste(cause_label_df$CODE_CLASS, cause_label_df$resp_vars, cause_label_df$CODE_VALUE)
    }

    if (expand_codes) {
      s_cause_label_df <- split(cause_label_df,
            list(cause_label_df$CODE_VALUE)) # , cause_label_df$CODE_CLASS
      warn_expand <- function(cldf) {
        my_labels <- cldf[!cldf$AUTO, CODE_LABEL, TRUE]
        my_labels <- my_labels[!is.na(my_labels)]
        if (!suppressWarnings && length(unique(my_labels)) == 1) {
          util_message("Would use label %s for all values coded with %s",
                       dQuote(unique(my_labels)),
                       dQuote(unique(cldf$CODE_VALUE)),
                       applicability_problem = TRUE)
        }
      }
      lapply(s_cause_label_df, warn_expand)
    }
    invisible(list(cause_label_df = cause_label_df))
}
