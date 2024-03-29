#' Utility function ensuring valid labels and variable names
#'
#' Valid labels should not be empty, be unique and do not exceed a certain
#' length. This also applies to the VAR_NAMES. When adjusting the VAR_NAMES, we
#' have to ensure that we do not introduce additional matches or mismatches
#' between the column names of the study data and the VAR_NAMES column in the
#' metadata.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
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

util_ensure_label <- function(study_data, meta_data, label_col,
                              max_label_len = MAX_LABEL_LEN) {
  if (label_col != VAR_NAMES) {
    # First, ensure that VAR_NAMES are usable.
    ensure_var_names <- util_ensure_label(study_data = study_data,
                                          meta_data = meta_data,
                                          label_col = VAR_NAMES)
    study_data <- ensure_var_names$study_data
    meta_data <- ensure_var_names$meta_data
  }
  if (!is.data.frame(meta_data)) {
    if (is.data.frame(study_data)) {
      original_label <- colnames(study_data)
    } else {
      return(list("study_data" = study_data,
                  "meta_data" = meta_data,
                  "label_modification_text" = NULL,
                  "label_modification_table" = NULL
      ))
    }
  } else {
    original_label <- meta_data[[label_col]]
  }
  N_label <- length(original_label)
  modified_label <- original_label
  label_modification_text <- NULL
  label_modification_table <- data.frame(
    "Original label" = original_label,
    "Modified label" = vector(mode = "character", length = N_label),
    "Reason" = vector(mode = "character", length = N_label),
    check.names = FALSE
  )
  if (is.data.frame(study_data)) {
    original_colnames <- colnames(study_data)
  } else {
    original_colnames <- "none"
  }
  modified_colnames <- original_colnames

  # check for missing labels ---------------------------------------------------
  if (any(util_empty(modified_label)) | any(util_empty(modified_colnames))) {
    ind_no_label <- which(util_empty(modified_label))
    if (length(ind_no_label) > 0) {
      util_warning(c("Some variables have no label in %s in %s.",
                     "Labels are required to create a report,",
                     "that's why missing labels will be replaced",
                     "provisionally. Please add missing labels in",
                     "your metadata."),
                   dQuote(label_col),
                   sQuote("meta_data"),
                   applicability_problem = TRUE
      )
    }
    if (label_col == VAR_NAMES) {
      ind_no_colnames <- which(util_empty(modified_colnames))
      # generate new variable names that will stand out in the report
      new_label <- paste("NO_VAR_NAME", sprintf(
        paste0("%0",
               # estimate the required number of digits:
               ceiling(log10(length(ind_no_label) + length(ind_no_colnames))),
               "d"),
        seq_len(length(ind_no_label) + length(ind_no_colnames))), sep = "_")
      # check for possible duplication
      while (any(new_label %in% c(modified_label, modified_colnames))) {
        # estimate the required number of digits:
        nn <- ceiling(log10(max(N_label, length(modified_colnames))))
        new_label <- paste("NO_VAR_NAME", sprintf(
          paste0("%0", nn + 2, "d"),
          sample(1:10^(nn + 1), # sample a random number
                 # the set from which we sample is larger than the number of
                 # variables, so even if there are already similar labels in
                 # use, there are most likely enough unassigned
                 # numbers left
                 size = length(ind_no_label) + length(ind_no_colnames),
                 replace = FALSE)), sep = "_")
      }
      new_label <- sort(new_label)
      # apply colnames, if needed
      if (length(ind_no_colnames) > 0) {
        modified_colnames[ind_no_colnames] <-
          new_label[(length(ind_no_label) + 1):length(new_label)]
        new_label <- new_label[seq_len(length(ind_no_label))]
      }
    } else { # i.e., label_col != VAR_NAMES
      if (VAR_NAMES %in% colnames(meta_data) &&
          !any(util_empty(meta_data[[VAR_NAMES]][ind_no_label])) &&
          all(nchar(meta_data[[VAR_NAMES]][ind_no_label]) < max_label_len)) {
        # use VAR_NAMES as labels, if possible
        new_label <- meta_data[[VAR_NAMES]][ind_no_label]
      } else {
        # replace empty label by a generic one that will stand out in the report
        new_label <- paste("NO LABEL", sprintf(
          paste0("%0",
                 # estimate the required number of digits:
                 ceiling(log10(length(ind_no_label))),
                 "d"),
          seq_len(length(ind_no_label))))
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
                 size = length(ind_no_label),
                 replace = FALSE)))
      }
    }
    # apply new labels
    if (length(ind_no_label) > 0) {
      modified_label[ind_no_label] <- new_label
      label_modification_text <-
        paste0(paste(sQuote(new_label), collapse = ", "),
               ifelse(length(ind_no_label) > 1,
                      paste(" were introduced for",
                            length(ind_no_label),
                            "missing labels."),
                      " was introduced for one missing label."))
      label_modification_table$Reason[ind_no_label] <- "No label specified."
    }
  }

  # check for duplicated labels ------------------------------------------------
  if (any(duplicated(modified_label)) | any(duplicated(modified_colnames))) {
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

    if (label_col == VAR_NAMES) {
      dupl_cn <- unique(modified_colnames[which(duplicated(modified_colnames))])
      dupl_both <- unique(c(dupl_lab, dupl_cn))
      for (ll in dupl_both) {
        ilab <- which(modified_label == ll)
        icn <- which(modified_colnames == ll)
        if (length(ilab) == 0 | length(icn) == 0) {
          if (length(ilab) + length(icn) > 2) {
            # The label occurs in only one place, but is duplicated more than
            # once, so we need to add numbers to discriminate duplicates.
            label_suffix <- c("", # the first occurrence does not get a suffix
                              paste0("_DUPLICATE_", sprintf(
                                paste0("%0",
                                       # estimate the required number of digits:
                                       ceiling(log10(
                                         max(length(ilab), length(icn)) - 1)),
                                       "d"),
                                seq_len(max(length(ilab), length(icn)) - 1))))
          } else {
            label_suffix <- c("", "_DUPLICATE")
          }
        } else {
          if (length(ilab) + length(icn) > 3) {
            # The label occurs in both places and is also duplicated more than
            # once, so we need to add numbers to discriminate duplicates.
            label_suffix <- paste0("_DUPLICATE_", sprintf(
              paste0("%0",
                     # estimate the required number of digits:
                     ceiling(log10(length(ilab) + length(icn) - 2)),
                     "d"),
              seq_len(length(ilab) + length(icn) - 2)))
            label_suffix <- c("",
                              label_suffix[seq_len(length(ilab) - 1)],
                              "",
                              label_suffix[length(ilab):length(label_suffix)])
          } else {
            if (length(ilab) == 2) {
              label_suffix <- c("", "_DUPLICATE", "")
            } else {
              label_suffix <- c("", "", "_DUPLICATE")
            }
          }
        }
        new_label <- trimws(paste0(ll, label_suffix))
        ind_unchanged <- which(nchar(label_suffix) == 0)
        # check for possible duplication
        temp_label <- c(modified_colnames, modified_label)
        while (any(new_label[-ind_unchanged] %in% temp_label)) {
          jj <- setdiff(which(new_label %in% temp_label), ind_unchanged)
          # estimate the required number of digits:
          nn <- ceiling(log10(max(N_label, length(modified_colnames))))
          new_label[jj] <- paste(new_label[jj], sprintf(
            paste0("%0", nn + 2, "d"),
            sample(1:10^(nn + 1), # sample a random number
                   # the set from which we sample is larger than the number of
                   # variables, so even if there are already similar labels in
                   # the study data, there are most likely enough unassigned
                   # numbers left
                   size = length(jj),
                   replace = FALSE), sep = "_"))
        }
        modified_label[ilab] <- new_label[seq_len(length(ilab))]
        modified_colnames[icn] <- new_label[(length(ilab)+1):length(new_label)]
      }
    } else { # i.e., label_col != VAR_NAMES
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

  # if (any(grepl("\\(|\\)", modified_label))) {
  #   ind_brackets <- which(grepl("\\(|\\)", modified_label))
  #   modified_label[ind_brackets] <-
  #     gsub("\\(", "\u2768", modified_label[ind_brackets])
  #  modified_label[ind_brackets] <-
  #     gsub("\\)", "\u2769", modified_label[ind_brackets])
  #   label_modification_text <- paste(label_modification_text,
  #                                    "Round brackets were replaced by",
  #                                    "unicode characters.")
  #   label_modification_table$Reason[ind_brackets] <- trimws(paste(
  #     label_modification_table$Reason[ind_brackets],
  #     "Round brackets were replaced by unicode characters."))
  # }

  # abbreviate() warns on non-ASCII characters, by default.
  my_abbreviate <- function(names.arg, method = "both.sides", ...) {
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
  if (any(nchar(modified_label) > max_label_len) |
      any(nchar(modified_colnames) > max_label_len)) {
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
                   applicability_problem = TRUE
      )
    }

    if (label_col == VAR_NAMES) {
      ind_long_colnames <- which(nchar(modified_colnames) > max_label_len)
      long_all <- c(modified_label[ind_long_label],
                    modified_colnames[ind_long_colnames])
      # remove punctuation from labels, if possible, before abbreviating (gives
      # more readable abbreviations)
      new_label <- unname(my_abbreviate(gsub("[[:punct:]]+", replacement = " ",
                                          long_all), minlength = max_label_len))
      if (any(util_empty(new_label))) {
        new_label <- unname(my_abbreviate(long_all, minlength = max_label_len))
      }
      label_modification_table$Reason[ind_long_label] <- trimws(
        paste(label_modification_table$Reason[ind_long_label],
              "The label is too long.")
      )
      # check for possible duplication between abbreviated labels and the
      # other, unchanged labels
      temp_label <- modified_label
      temp_label[ind_long_label] <- new_label[1:length(ind_long_label)]
      temp_colnames <- modified_colnames
      temp_colnames[ind_long_colnames] <-
        new_label[(length(ind_long_label) + 1):length(new_label)]
      while (any(duplicated(temp_label)) | any(duplicated(temp_colnames))) {
        # add the duplicated labels to the selection of labels that should
        # be adapted by 'abbreviate'
        ind_long_label <- c(ind_long_label,
                            which(modified_label %in% new_label))
        ind_long_label <- sort(unique(ind_long_label))
        ind_long_colnames <- c(ind_long_colnames,
                               which(modified_colnames %in% new_label))
        ind_long_colnames <- sort(unique(ind_long_colnames))
        long_all <- c(modified_label[ind_long_label],
                      modified_colnames[ind_long_colnames])
        # rerun the 'abbreviate' function to get shortened, unique labels
        new_label <- unname(my_abbreviate(gsub("[[:punct:]]+", replacement = " ",
                                            long_all), minlength = max_label_len))
        if (any(util_empty(new_label))) {
          new_label <- unname(my_abbreviate(long_all, minlength = max_label_len))
        }
        temp_label[ind_long_label] <- new_label[1:length(ind_long_label)]
        temp_colnames[ind_long_colnames] <-
          new_label[(length(ind_long_label) + 1):length(new_label)]
      }

      modified_label <- temp_label
      modified_colnames <- temp_colnames

      if (any(util_empty(label_modification_table$Reason[ind_long_label]))) {
        ind_long_label2 <- intersect(
          ind_long_label,
          which(util_empty(label_modification_table$Reason))
        )
        label_modification_table$Reason[ind_long_label2] <- trimws(
          paste(label_modification_table$Reason[ind_long_label2],
                "The label was identical to an abbreviated label.")
        )
      }

      if (is.data.frame(meta_data)) {
        if (exists("ind_no_label")) {
          # If we store the original labels or variable names (only needed if
          # they are abbreviated), then we have to replace empty entries,
          # otherwise we will run into errors later with 'util_find_var_by_meta'
          # running 'util_map_labels' for 'ORIGINAL_VAR_NAMES'.
          original_label[ind_no_label] <- "(missing)"
        }
        meta_data[["ORIGINAL_VAR_NAMES"]] <- original_label
      }
    } else { # i.e., label_col != VAR_NAMES
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

      if (is.data.frame(meta_data)) {
        if (exists("ind_no_label")) {
          # If we store the original labels or variable names (only needed if they
          # are abbreviated), then we have to replace empty entries, otherwise we
          # will run into errors later with 'util_find_var_by_meta'
          # running 'util_map_labels' for 'ORIGINAL_LABEL'.
          original_label[ind_no_label] <- "(missing)"
        }
        meta_data[["ORIGINAL_LABEL"]] <- original_label
      }
    }

    if (length(ind_long_label) > 0) {
      label_modification_text <- c(
        label_modification_text,
        paste0(paste(sQuote(modified_label[ind_long_label]), collapse = ", "),
               ifelse(length(ind_long_label) > 1,
                      paste0(" were introduced as abbreviated labels for ",
                             paste(sQuote(original_label[ind_long_label]),
                                   collapse = ", "), "."),
                      paste0(
                        " was introduced as an abbreviation for the label ",
                             sQuote(original_label[ind_long_label]), ".")
               )
        )
      )
    }
  }

  # apply modified labels now that all checks are done -------------------------
  if (is.data.frame(study_data)) {
    colnames(study_data) <- modified_colnames
  }

  if (is.data.frame(meta_data)) {
    meta_data[[label_col]] <- modified_label
  }

  label_modification_table[, 2] <- modified_label
  # only show entries with modifications
  label_modification_table <- label_modification_table[which(
    label_modification_table[, 1] != label_modification_table[, 2] |
      !util_empty(label_modification_table[, 3])
  ), ]

  return(list("study_data" = study_data,
              "meta_data" = meta_data,
              "label_modification_text" = trimws(paste(label_modification_text,
                                                       collapse = " ")),
              "label_modification_table" = label_modification_table
              ))
}
