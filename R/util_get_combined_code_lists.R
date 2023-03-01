util_get_combined_code_lists <- function(x,
                                         code_name,
                                         split_char = SPLIT_CHAR,
                                         mdf,
                                         label_col = VAR_NAMES,
                                         warning_if_no_list = TRUE,
                                         assume_consistent_codes = TRUE,
                                         have_cause_label_df = FALSE) {
  l <- lapply(code_name, function(cn) {
    lb <- sub("_.*?$", "", cn, perl = TRUE)
    .l <-
      util_get_code_list(x, cn, split_char, mdf,
                         label_col,
                         warning_if_no_list)
    if (!have_cause_label_df ||
        (!(lb %in% c("JUMP", "MISSING")))) {
      if (any(names(.l) == as.character(.l))) {
        if (assume_consistent_codes) {
          names(.l)[names(.l) == as.character(.l)] <-
            paste(lb, .l[names(.l) == as.character(.l)])
        } else {
          names(.l)[names(.l) == as.character(.l)] <-
            paste(lb, x, .l[names(.l) == as.character(.l)])
        }
      }
    }
    setNames(as.character(.l), nm = names(.l))
  })
  unlist(l, recursive = FALSE)
}
