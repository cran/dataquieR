#' Utility function ensuring valid labels and variable names
#'
#' Valid labels should not be empty, be unique and do not exceed a certain
#' length.
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param max_label_len [integer] maximum length for the labels, defaults to
#'                                `r MAX_LABEL_LEN`.
#'
#' @return a list containing the study data, possibly with adapted column names,
#'         the metadata, possibly with adapted labels, and a string and a
#'         table informing about the changes
#'
#' @keywords internal

util_ensure_label <- function(meta_data, label_col,
                              max_label_len = MAX_LABEL_LEN) { # TODO: Add somehow also to util_validate_known_meta, after VAR_NAMES are not changed any more
  util_expect_scalar(label_col, check_type = is.character)
  util_expect_data_frame(meta_data, col_names = c(label_col, VAR_NAMES))

  if (any(util_empty(meta_data[[VAR_NAMES]]))) {
    util_warning(c("Need %s in %s, for all variables, some are missing.",
                 "I'll discard all %s without %s."),
               sQuote(VAR_NAMES),
               sQuote("meta_data"),
               sQuote("meta_data"),
               sQuote(VAR_NAMES),
               applicability_problem = TRUE)
    meta_data <- meta_data[!util_empty(meta_data[[VAR_NAMES]]), , FALSE]
  }
  original <- list(
    meta_data = meta_data,
    label_col = label_col
  )
  if (label_col == VAR_NAMES) {
    i <- 0
    new_label_col <- label_col
    while (new_label_col %in% colnames(meta_data)) {
        i <- i + 1
        new_label_col <- paste0(label_col, "_", i)
    }
    meta_data[[new_label_col]] <-
      meta_data[[label_col]]
    label_col <- new_label_col
  }
  N_label <- length(original$meta_data[[original$label_col]])
  modified_label <- original$meta_data[[original$label_col]]
  label_modification_text <- NULL
  label_modification_table <- data.frame(
    "Original label" = original$meta_data[[original$label_col]],
    "Modified label" = vector(mode = "character", length = N_label),
    "Reason" = vector(mode = "character", length = N_label),
    check.names = FALSE
  )

  # check for missing labels ---------------------------------------------------
  if (any(ind_no_label <- util_empty(modified_label))) {
    if (sum(ind_no_label) > 0) {
      util_warning(c("Some variables have no label in %s in %s.",
                     "Labels are required to create a report,",
                     "that's why missing labels will be replaced",
                     "provisionally. Please add missing labels in",
                     "your metadata."),
                   dQuote(label_col),
                   sQuote("meta_data"),
                   applicability_problem = TRUE
      )
      new_label <- meta_data[[VAR_NAMES]][ind_no_label]
    } else {
      new_label <- character(0)
    }

    # check for possible duplication
    while (any(new_label %in% modified_label)) {
      # estimate the required number of digits:
      nn <- ceiling(log10(N_label))
      new_label <- paste("NO LABEL", sprintf(
        paste0("%0", nn + 2, "d"),
        sample(1:10^(nn + 1), # sample a random number
               # the set from which we sample is larger than the number of
               # variables, so even if there are already similar labels in
               # the study data, there are most likely enough unassigned
               # numbers left
               size = sum(ind_no_label),
               replace = FALSE)))
    }

    # apply new labels
    if (sum(ind_no_label) > 0) {
      modified_label[ind_no_label] <- new_label
      label_modification_text <-
        paste0(paste(sQuote(new_label), collapse = ", "),
               ifelse(sum(ind_no_label) > 1,
                      paste(" were introduced for",
                            sum(ind_no_label),
                            "missing labels."),
                      " was introduced for one missing label."))
      label_modification_table$Reason[ind_no_label] <- "No label specified."
    }
  }

  # check for duplicated labels ------------------------------------------------
  if (any(duplicated(modified_label))) {
    dupl_lab <- unique(modified_label[which(duplicated(modified_label))])
    if (length(dupl_lab) > 0) {
      util_warning(c("Some variables have duplicated labels in %s in %s.",
                     "Unique labels are required to create a report,",
                     "that's why duplicated labels will be replaced",
                     "provisionally. Please modify the labels in",
                     "your metadata or select a suitable column."),
                   dQuote(label_col),
                   sQuote("meta_data"),
                   applicability_problem = TRUE
      )
    }
    mod_lab_before <- modified_label

    for (ll in dupl_lab) {
      ii <- which(modified_label == ll)
      ndupl_ll <- length(ii)
      if (ndupl_ll > 2) {
        # label is duplicated more than once, so we need to add numbers
        label_suffix <- c("", # the first occurrence does not get a suffix
                          paste("DUPLICATE", sprintf(
                            paste0("%0",
                                   # estimate the required number of digits:
                                   ceiling(log10(ndupl_ll - 1)),
                                   "d"),
                            seq_len(ndupl_ll - 1))))
      } else {
        label_suffix <- c("", "DUPLICATE")
      }
      new_label <- trimws(paste(ll, label_suffix))
      # check for possible duplication
      while (any(new_label[-1] %in% modified_label)) {
        jj <- which(new_label %in% modified_label)[-1]
        # estimate the required number of digits:
        nn <- ceiling(log10(N_label))
        new_label[jj] <- paste(new_label[jj], sprintf(
          paste0("%0", nn + 2, "d"),
          sample(1:10^(nn + 1), # sample a random number
                 # the set from which we sample is larger than the number of
                 # variables, so even if there are already similar labels in
                 # the study data, there are most likely enough unassigned
                 # numbers left
                 size = length(jj),
                 replace = FALSE)))
      }
      modified_label[ii] <- new_label
    }

    ind_dupl <- which(mod_lab_before != modified_label)
    if (length(ind_dupl) > 0) {
      label_modification_text <- c(
        label_modification_text,
        paste0(
          paste(sQuote(modified_label[ind_dupl]), collapse = ", "),
          ifelse(length(ind_dupl) > 1,
                 " were introduced for duplicated labels.",
                 " was introduced for one duplicated label.")
        ))
      label_modification_table$Reason[ind_dupl] <- trimws(
        paste(label_modification_table$Reason[ind_dupl], "Duplicated label."))
    }
  }

  # abbreviate() warns on non-ASCII characters, by default.
  my_abbreviate <- function(names.arg, method = "both.sides", ...) {  # FIXME: this qualifies for an own function for abbreviating, because we need it for value labels, too
    x <- names.arg
    x_words <- strsplit(x, "\\s+")
    uc_first_words <-
      lapply(x_words, function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1));
        x
      })
    uc_first_words <-
      vapply(uc_first_words, paste, collapse = " ", FUN.VALUE = character(1))
    suppressWarnings(abbreviate(names.arg = uc_first_words, method = method,
                                ...))
  }

  # abbreviate long labels -----------------------------------------------------
  if (any(nchar(modified_label) > max_label_len)) {
    ind_long_label <- which(nchar(modified_label) > max_label_len)
    if (length(ind_long_label) > 0) {
      util_warning(c("Some variables have labels with more than %d characters",
                     "in %s in %s.",
                     "This will cause suboptimal outputs and possibly also",
                     "failures when rendering the report,",
                     "due to issues with the maximum length of file names",
                     "in your operating system or file system.",
                     "This will be fixed provisionally. Please shorten",
                     "your labels or choose another label column."),
                   max_label_len,
                   dQuote(label_col),
                   sQuote("meta_data"),
                   applicability_problem = TRUE,
                   additional_classes = LONG_LABEL_EXCEPTION
      )
    }

    # remove punctuation from labels, if possible, before abbreviating (gives
    # more readable abbreviations)
    new_label <- unname(my_abbreviate(gsub("[[:punct:]]+", replacement = " ",
                                        modified_label[ind_long_label]),
                                   minlength = max_label_len))
    if (any(util_empty(new_label))) {
      new_label <- unname(my_abbreviate(modified_label[ind_long_label],
                                     minlength = max_label_len))
    }
    label_modification_table$Reason[ind_long_label] <- trimws(
      paste(label_modification_table$Reason[ind_long_label],
            "The label is too long.")
    )
    # check for possible duplication between abbreviated labels and the
    # other, unchanged labels
    temp_label <- modified_label
    temp_label[ind_long_label] <- new_label
    while (any(duplicated(temp_label))) {
      # add the duplicated labels to the selection of labels that should
      # be adapted by 'abbreviate'
      ind_long_label <- c(ind_long_label,
                          which(modified_label %in% new_label))
      ind_long_label <- sort(unique(ind_long_label))
      # rerun the 'abbreviate' function to get shortened, unique labels
      new_label <- unname(my_abbreviate(gsub("[[:punct:]]+", replacement = " ",
                                          modified_label[ind_long_label]),
                                     minlength = max_label_len))
      if (any(util_empty(new_label))) {
        new_label <- unname(my_abbreviate(modified_label[ind_long_label],
                                       minlength = max_label_len))
      }
      temp_label[ind_long_label] <- new_label
    }

    modified_label <- temp_label

    if (any(util_empty(label_modification_table$Reason[ind_long_label]))) {
      ind_long_label2 <- intersect(ind_long_label,
                                   which(util_empty(
                                     label_modification_table$Reason)))
      label_modification_table$Reason[ind_long_label2] <- trimws(
        paste(label_modification_table$Reason[ind_long_label2],
              "The label was identical to an abbreviated label.")
      )
    }

    if (any(ind_no_label)) {
      # If we store the original labels or variable names (only needed if they
      # are abbreviated), then we have to replace empty entries, otherwise we
      # will run into errors later with 'util_find_var_by_meta'
      # running 'util_map_labels' for 'ORIGINAL_LABEL'.
      original$meta_data[[original$label_col]][ind_no_label] <- "(missing)"
    }
    meta_data[["ORIGINAL_LABEL"]] <- original$meta_data[[original$label_col]]

    if (length(ind_long_label) > 0) {
      label_modification_text <- c(
        label_modification_text,
        paste0(paste(sQuote(modified_label[ind_long_label]), collapse = ", "),
               ifelse(length(ind_long_label) > 1,
                      paste0(" were introduced as abbreviated labels for ",
                             paste(sQuote(original$meta_data[[original$label_col]][ind_long_label]),
                                   collapse = ", "), "."),
                      paste0(
                        " was introduced as an abbreviation for the label ",
                             sQuote(original$meta_data[[original$label_col]][ind_long_label]), ".")
               )
        )
      )
    }
  }

  # apply modified labels now that all checks are done -------------------------

  if (is.data.frame(meta_data)) {
    meta_data[[label_col]] <- modified_label
  }

  label_modification_table[, 2] <- modified_label
  # only show entries with modifications
  label_modification_table <- label_modification_table[which(
    label_modification_table[, 1] != label_modification_table[, 2] |
      !util_empty(label_modification_table[, 3])
  ), ]

  return(list(meta_data = meta_data,
              label_modification_text = trimws(paste(label_modification_text,
                                                       collapse = " ")),
              label_modification_table = label_modification_table,
              label_col = label_col
              ))
}
