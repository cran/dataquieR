#' Utility function ensuring valid labels
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list containing the metadata, possibly with adapted labels,
#'         and a string informing about the changes
#'

util_ensure_label <- function(meta_data, label_col) {
  label_modification_text <- NULL
  label_modification_table <- data.frame(
    "Original label" = meta_data[[label_col]],
    "Modified label" = vector(mode = "character", length = nrow(meta_data)),
    "Reason" = vector(mode = "character", length = nrow(meta_data)),
    check.names = FALSE
  )
  if (any(util_empty(meta_data[[label_col]]))) {
    util_warning(c("Some variables have no label in %s in %s.",
                   "Labels are required to create a report,",
                   "that's why missing labels will be replaced",
                   "provisionally. Please add missing labels in",
                   "your metadata."),
                 dQuote(label_col),
                 sQuote("meta_data"),
                 applicability_problem = TRUE
    )
    ind_no_label <- which(util_empty(meta_data[[label_col]]))
    # replace missing labels by VAR_NAMES, if possible
    if (label_col != VAR_NAMES &
        !any(util_empty(meta_data[[VAR_NAMES]][ind_no_label])) &
        all(nchar(meta_data[[VAR_NAMES]][ind_no_label]) < MAX_LABEL_LEN - 9)) {
      new_labels <- paste("NO LABEL",
                          meta_data[[VAR_NAMES]][ind_no_label])
    } else { # replace empty label by a generic variable name
      # generate new variable names that will stand out in the report
      new_labels <- paste("NO LABEL", sprintf(
        paste0("%0",
               # estimate the required number of digits:
               ceiling(log10(length(ind_no_label))),
               "d"),
        seq_len(length(ind_no_label))))
    }
    # check for possible duplication
    while (any(new_labels %in% meta_data[[label_col]])) {
      # estimate the required number of digits:
      nn <- ceiling(log10(nrow(meta_data)))
      new_labels <- paste("NO LABEL", sprintf(
        paste0("%0", nn + 2, "d"),
        sample(1:10^(nn + 1), # sample a random number
               # the set from which we sample is larger than the number of
               # variables, so even if there are already similar labels in
               # the study data, there are most likely enough unassigned
               # numbers left
               size = length(ind_no_label),
               replace = FALSE)))
    }
    # apply new labels
    meta_data[[label_col]][ind_no_label] <- new_labels
    label_modification_text <- paste0(paste(sQuote(new_labels), collapse = ", "),
                                  ifelse(length(ind_no_label) > 1,
                                         paste(" were introduced for",
                                               length(ind_no_label),
                                               "missing labels."),
                                         " was introduced for one missing label."))
    label_modification_table$Reason[ind_no_label] <- "No label specified."
  }

  if (any(duplicated(meta_data[[label_col]]))) {
    util_warning(c("Some variables have duplicated labels in %s in %s.",
                   "Unique labels are required to create a report,",
                   "that's why duplicated labels will be replaced",
                   "provisionally. Please modify the labels in",
                   "your metadata or select a suitable column, such as %s."),
                 dQuote(label_col),
                 sQuote("meta_data"),
                 sQuote(VAR_NAMES),
                 applicability_problem = TRUE
    )
    temp_labels <- meta_data[[label_col]]
    names_dupl_label <- temp_labels[which(duplicated(temp_labels))]
    for (ll in names_dupl_label) {
      ii <- which(temp_labels == ll)
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
      new_labels <- trimws(paste(ll, label_suffix))
      # check for possible duplication
      while (any(new_labels[-1] %in% temp_labels)) {
        jj <- which(new_labels %in% temp_labels)[-1]
        # estimate the required number of digits:
        nn <- ceiling(log10(nrow(meta_data)))
        new_labels[jj] <- paste(new_labels[jj], sprintf(
          paste0("%0", nn + 2, "d"),
          sample(1:10^(nn + 1), # sample a random number
                 # the set from which we sample is larger than the number of
                 # variables, so even if there are already similar labels in
                 # the study data, there are most likely enough unassigned
                 # numbers left
                 size = length(jj),
                 replace = FALSE)))
      }
      temp_labels[ii] <- new_labels
    }

    ind_dupl <- which(temp_labels != meta_data[[label_col]])
    label_modification_text <- c(
      label_modification_text,
      paste0(paste(sQuote(
        temp_labels[ind_dupl]),
        collapse = ", "),
        ifelse(length(ind_dupl) > 1,
               " were introduced for duplicated labels.",
               " was introduced for one duplicated label.")))
    label_modification_table$Reason[ind_dupl] <- trimws(
      paste(label_modification_table$Reason[ind_dupl], "Duplicated label."))
    # apply modified labels
    meta_data[[label_col]] <- temp_labels
  }

  # if (any(grepl("\\(|\\)", meta_data[[label_col]]))) {
  #   ind_brackets <- which(grepl("\\(|\\)", meta_data[[label_col]]))
  #   meta_data[[label_col]][ind_brackets] <-
  #     gsub("\\(", "\u2768", meta_data[[label_col]][ind_brackets])
  #   meta_data[[label_col]][ind_brackets] <-
  #     gsub("\\)", "\u2769", meta_data[[label_col]][ind_brackets])
  #   label_modification_text <- paste(label_modification_text,
  #                                    "Round brackets were replaced by",
  #                                    "unicode characters.")
  #   label_modification_table$Reason[ind_brackets] <- trimws(paste(
  #     label_modification_table$Reason[ind_brackets],
  #     "Round brackets were replaced by unicode characters."))
  # }

  if (any(nchar(meta_data[[label_col]]) > MAX_LABEL_LEN)) {
    util_warning(c("Some variables have labels with more than %d characters",
                   "in %s in %s.",
                   "This will cause suboptimal outputs and possibly also",
                   "failures when rendering the report,",
                   "due to issues with the maximum length of file names",
                   "in your operating system or file system.",
                   "This will be fixed provisionally. Please shorten",
                   "your labels or choose another label column."),
                 MAX_LABEL_LEN,
                 dQuote(label_col),
                 sQuote("meta_data"),
                 applicability_problem = TRUE
    )
    ind_long_label <- which(nchar(meta_data[[label_col]]) > MAX_LABEL_LEN)
    new_labels <- abbreviate(meta_data[[label_col]][ind_long_label])
    # check for possible duplication between abbreviated labels and the
    # other, unchanged labels
    temp_labels <- meta_data[[label_col]]
    temp_labels[ind_long_label] <- new_labels
    while (any(duplicated(temp_labels))) {
      # add the duplicated labels to the selection of labels that should
      # be adapted by 'abbreviate'
      ind_long_label <- c(ind_long_label,
                          which(meta_data[[label_col]] %in% new_labels))
      ind_long_label <- sort(unique(ind_long_label))
      # rerun the 'abbreviate' function to get shortened, unique labels
      new_labels <- abbreviate(meta_data[[label_col]][ind_long_label])
      temp_labels[ind_long_label] <- new_labels
    }
    label_modification_text <- c(
      label_modification_text,
      paste0(paste(sQuote(new_labels), collapse = ", "),
             ifelse(length(ind_long_label) > 1,
                    paste0(" were introduced as abbreviated labels for ",
                           paste(sQuote(meta_data[[label_col]][ind_long_label]),
                                 collapse = ", "), "."),
                    paste0(" was introduced as an abbreviation for the label ",
                           sQuote(meta_data[[label_col]][ind_long_label]), ".")
             )
      )
    )
    label_modification_table$Reason[ind_long_label] <- trimws(
      paste(label_modification_table$Reason[ind_long_label],
            "The label is too long.")
    )

    # apply modified labels
    meta_data[[label_col]][ind_long_label] <- new_labels
  }

  label_modification_table[, 2] <- meta_data[[label_col]]
  label_modification_table <- label_modification_table[which(
    label_modification_table[, 1] != label_modification_table[, 2] |
      !util_empty(label_modification_table[, 3])
  ), ]

  return(list("meta_data" = meta_data,
              "label_modification_text" = trimws(paste(label_modification_text, collapse = " ")),
              "label_modification_table" = label_modification_table
              ))
}
