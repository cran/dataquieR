util_map_to_other_metrics <- function(sumtab,
                                      meta_data,
                                      label_col,
                                      add_attribs) {

  if (.called_in_pipeline &&
      COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
    cvr <-
      util_map_labels(sumtab$Variables,
                      meta_data = meta_data,
                      to = COMPUTED_VARIABLE_ROLE,
                      from = label_col,
                      ifnotfound = NA,
                      warn_ambiguous = FALSE)

    if (is.environment(add_attribs) && length(cvr) == 1 && is.character(cvr) &&
        !all(util_empty(cvr))) {
      add_attribs[[COMPUTED_VARIABLE_ROLE]] <- cvr
    }

    if (nrow(sumtab) == 1 && length(cvr) == 1 && is.character(cvr) &&
        !all(util_empty(cvr))) {

      cvim <-
        util_get_concept_info("computed_vars_ind_mapping",
                              get("computed_role") == cvr,
                              c("needle", "replacement"), drop = FALSE)

      sumtab <- sumtab[, c("Variables",
                           grep(paste0(".*(",
                                       paste(cvim$needle, collapse = "|"),
                                       ").*"),
                                colnames(sumtab),
                                value = TRUE, perl = TRUE)), FALSE]
      for (i in seq_len(nrow(cvim))) {
        needle <- cvim$needle[i]
        replacement <- cvim$replacement[i]

        colnames(sumtab) <- gsub(
          paste0("^([A-Z1-9]+_)", needle, "(.*)$"),
          paste0("\\1", replacement, "\\2"),
          colnames(sumtab),
          perl = TRUE
        )
      }

    }
  }

  return(sumtab)
}
